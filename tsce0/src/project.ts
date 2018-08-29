import { observable, computed } from "mobx"
import { Name, UUID } from "./names"
import * as M from "./module"
import * as S from "./symbols"

/** Components define either libraries or applications. */
export enum Type { LIB, APP }

/** A dependency on one or more components of a project. */
export type Depend = {
  source :string
  puuid :UUID
  cuuids :UUID[]
}

/** The serialized form of a component. */
type ComponentJson = {
  uuid :UUID,
  name :string,
  type :Type,
  depends :Depend[],
  modules :UUID[]
}

/** The serialized form of a project. */
type ProjectJson = {
  uuid :UUID,
  source :string,
  name :string,
  components :ComponentJson[]
}

/** Used to resolve component dependencies. */
export interface Resolver {
  /** Resolves and returns the project identified by `uuid`. `undefined` is returned if the project
    * cannot be located. This is indicative of a corrupted project or workspace. */
  resolveProject (uuid :UUID) :Project|void
}

/** A project component contains one or more modules and is either an application or library. The
  * purpose of an application is to generate an executable. The purpose of a library is to provide
  * code that can be used by other components. */
export class Component {

  /** The human readable name of this component. */
  @observable name :string

  /** The modules that make up this component. */
  @observable modules :M.Module[] = observable.array([])

  /** The project/components on which this component depends. */
  @observable depends :Depend[] = observable.array([])

  /** A scope used to complete symbols across all modules in this component (and its depends). */
  readonly scope :ComponentScope

  constructor (readonly uuid :UUID, readonly type :Type, name :string, resolver :Resolver) {
    this.name = name
    this.scope = new ComponentScope(this, resolver)
  }

  deflate () :ComponentJson {
    const {uuid, type, name, depends} = this
    return {uuid, type, name, depends, modules: this.modules.map(m => m.uuid)}
  }
}

export class Project {

  /** A URL that identifies the DVCS source of this project. */
  @observable source :string

  /** The human readable name of this project. */
  @observable name :string

  /** The components that make up this project. */
  @observable components :Component[] = observable.array([])

  constructor (readonly uuid :UUID, source :string, name :string) {
    this.source = source
    this.name = name
  }

  /** Returns the component of this project with `uuid`, or `undefined`. */
  component (uuid :UUID) :Component|void {
    for (let comp of this.components) if (comp.uuid === uuid) return comp
  }

  deflate () :ProjectJson {
    const {uuid, source, name} = this
    return {uuid, source, name, components: this.components.map(c => c.deflate())}
  }

  toString () :string { return this.name }
}

class ModuleRootScope extends S.Scope {
  constructor (readonly cscope :ComponentScope, readonly muuid :UUID) { super() }

  lookup (kind :S.Kind, name :Name) :S.Symbol {
    throw new Error(`Illegal to resolve names lexically in mod root`)
  }

  toString () :string {
    return `<project:${this.cscope.comp.name}%${this.muuid}>`
  }

  _addCompletions (pred :(sym :S.Symbol) => Boolean, prefix :string, syms :S.Symbol[]) {
    // TODO: maintain some sort of more efficient index for all symbols in project?
    for (let mod of this.cscope.comp.modules) if (mod.uuid !== this.muuid) mod.scope.addCompletions(
      pred, prefix, syms)
    for (let dcomp of this.cscope.deps) dcomp.scope.addCompletions(pred, prefix, syms)
  }

}

class ComponentScope extends S.Scope {

  // TODO: do we want to implicitly search all of a component's transitive dependencies during
  // symbol completion? perhaps only dependencies specifically marked as re-exported (which would be
  // needed if any type appeared in the module's exported API)?

  // this comopnent's complete transitive dependency set
  @computed get deps () :Component[] {
    let deps :Component[] = []
    for (let dep of this.comp.depends) {
      const dproj = this.resolver.resolveProject(dep.puuid)
      if (dproj) {
        for (let cuuid of dep.cuuids) {
          const dcomp = dproj.component(cuuid)
          if (dcomp) deps.push(dcomp)
          else console.warn(`Component depend missing [comp=${this.comp}, cuuid=${cuuid}, ` +
                            `dep=${JSON.stringify(dep)}]`)
        }
      }
      else console.warn(`Project depend missing [proj=${this.comp}, ` +
                        `dep=${JSON.stringify(dep)}]`)
    }
    return deps
  }

  constructor (readonly comp :Component, private readonly resolver :Resolver) { super() }

  lookup (kind :S.Kind, name :Name) :S.Symbol {
    throw new Error(`Illegal to resolve names lexically in component`)
  }

  toString () :string {
    return `<project:${this.comp.name}>`
  }

  addCompletions (pred :(sym :S.Symbol) => Boolean, prefix :string, syms :S.Symbol[]) {
    // TODO: maintain some sort of more efficient index for all symbols in project?
    for (let mod of this.comp.modules) mod.scope.addCompletions(pred, prefix, syms)
  }

  _addCompletions (pred :(sym :S.Symbol) => Boolean, prefix :string, syms :S.Symbol[]) {
    this.addCompletions(pred, prefix, syms)
    for (let dcomp of this.deps) dcomp.scope.addCompletions(pred, prefix, syms)
  }
}

export function inflateProject (resolver :Resolver, json :ProjectJson,
                                modResolver :M.Resolver, modJsons :any[]) :Project {
  const modJsonMap = new Map()
  for (let modJson of modJsons) modJsonMap.set(modJson.uuid, modJson)
  const proj = new Project(json.uuid, json.source, json.name)
  for (let cjson of json.components) {
    const comp = new Component(cjson.uuid, cjson.type, cjson.name, resolver)
    for (let djson of cjson.depends) comp.depends.push(djson)
    for (let muuid of cjson.modules) {
      const mjson = modJsonMap.get(muuid)
      if (mjson) comp.modules.push(
        M.inflateMod(new ModuleRootScope(comp.scope, mjson.uuid), modResolver, mjson))
      else console.warn(`Component references unknown module [comp=${JSON.stringify(cjson)}, ` +
                        `muuid=${muuid}]`)
    }
    proj.components.push(comp)
  }
  return proj
}
