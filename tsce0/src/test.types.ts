import { Name } from "./names"

import * as C from "./constants"
import * as F from "./format"
import * as P from "./prefab"
import * as S from "./symbols"
import * as ST from "./store"
import * as T from "./trees"
import * as TP from "./types"
import * as W from "./workspace"

let testSymId = 0

class TestSym extends S.Symbol {
  constructor (kind :S.Kind, name :Name) { super(kind, "none", ++testSymId, name) }
  get owner () :S.Symbol { return this }
  get type () :TP.Type { return TP.hole }
}

const boolType = new TP.Scalar(C.Tag.Bool, 1)

const xV = new TestSym("term", "x")
// const yV = new TestSym("term", "y")
// const fV = new TestSym("term", "f")

it("splits contexts", () => {
  const aUV = new TP.UVar(new TestSym("type", "a"))
  const bEV = new TP.EVar("b")
  const xVAssump = new TP.NAssump(xV, boolType)
  const ctx = new TP.Context(new TP.Tracer()).extend(xVAssump).extend(bEV).extend(aUV)
  const splits = ctx.split(bEV)
  if (!splits) fail("Failed to split context on `b`")
  else {
    const [post, pre] = splits
    expect(post.notes).toEqual([aUV])
    expect(pre.notes).toEqual([xVAssump])
  }
})

const tb = new T.TreeBuilder()
const falseJson = tb.mkLit({tag: C.Tag.Bool, value: "false"})

function makeTestMod (defsJson :any[]) :Module {
  const store = new ST.MemoryStore(new Map())
  P.addTestProject(store, [
    // id ∀A a:A -> A = a
    tb.mkTermDef("id", 10, tb.mkAll("A", 1, tb.mkAbs("a", 2, tb.mkTRef("l1"), tb.mkAsc(
      tb.mkTRef("l1"), tb.mkRef("l2"))))),
    // id false
    tb.mkTermDef("idAtFalse", 11, tb.mkApp(tb.mkRef("m10"), falseJson)),
    // id id
    tb.mkTermDef("idAtFalse", 11, tb.mkApp(tb.mkRef("m10"), tb.mkRef("m10"))),
  ])

  const wspace = new W.WorkspaceStore(store)
  wspace.projects.push(P.primProject)
  const testProj = wspace.openProject(P.testProjUUID)
  if (!testProj) throw new Error(`Test project failed to load`)

  return testProj.components[0].modules[0]
}

it("types id", () => {
  const testMod = makeTestMod([
    // id ∀A a:A -> A = a
    tb.mkTermDef("id", 10, tb.mkAll("A", 1, tb.mkAbs("a", 2, tb.mkTRef("l1"), tb.mkAsc(
      tb.mkTRef("l1"), tb.mkRef("l2"))))),
    // id false
    tb.mkTermDef("idAtFalse", 11, tb.mkApp(tb.mkRef("m10"), falseJson)),
    // id id
    tb.mkTermDef("idAtFalse", 11, tb.mkApp(tb.mkRef("m10"), tb.mkRef("m10"))),
  ])
  for (let defSym of testMod.defs) {
    const defTree= testMod.tree(defSym)
    console.log(`${defSym.sym} :: ${defTree.signature} :: ${defTree.treeType}`)
    const {elem} = F.format(testMod, defTree, new T.Path(), {showSigs: true})
    console.log(elem.debugShow().join("\n"))
  }
})
