import { UUID } from "./names"
import * as M from "./module"
import * as P from "./prefab"
import * as S from "./symbols"
import * as ST from "./store"
import * as W from "./workspace"

const store = new ST.MemoryStore(new Map())
P.seedTestProject(store)
const wspace = new W.WorkspaceStore(store)
wspace.projects.push(P.primProject)
const testProj = wspace.openProject(P.testProjUUID)
if (!testProj) throw new Error(`Test project failed to load`)

const modResolver :M.Resolver = {
  resolveModule :(uuid :UUID) => P.primProject.components[0].modules[0]
}

const testMod = testProj.components[0].modules[0]

it("deflate/inflate/deflate round trip", () => {
  const modJson = testMod.deflate()
  const modJsonStr = JSON.stringify(modJson, undefined, " ")

  const infMod = M.inflateMod(S.emptyScope, modResolver, modJson)
  const infModJson = infMod.deflate()
  const infModJsonStr = JSON.stringify(infModJson, undefined, " ")
  expect(infModJsonStr).toEqual(modJsonStr)
})
