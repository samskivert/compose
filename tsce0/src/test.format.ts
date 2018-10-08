import * as T from "./trees"
import * as M from "./module"
import * as P from "./prefab"
import * as F from "./format"
import * as S from "./store"
import * as W from "./workspace"

const store = new S.MemoryStore(new Map())
P.seedTestProject(store)
const wspace = new W.WorkspaceStore(store)
wspace.projects.push(P.primProject)
const testProj = wspace.openProject(P.testProjUUID)
if (!testProj) throw new Error(`Test project failed to load`)

const testMod = testProj.components[0].modules[0]
const testSym = testMod.symById(10)
const testTree = testSym && testMod.tree(testSym as M.DefSym)

it("formats things", () => {
  if (testTree instanceof T.DefTree) {
    console.log(testTree.debugShow().join("\n"))
    const {elem} = F.format(testMod, testTree, new T.Path())
    console.dir(elem)
    console.log(elem.debugShow().join("\n"))
  } else {
    console.log(`Invalid test sym? ${testSym} / ${testTree}`)
  }
})

// it("edit expr: app", () => {
//   const pants = new T.Ref("pants")
//   const legs = new T.Ref("legs")
//   const orig = new T.App(pants, legs)
//   expect(orig.editExpr(old => new T.Ref("socks"), [0])).
//     toEqual(new T.App(new T.Ref("socks"), legs))
//   expect(orig.editExpr(old => new T.Ref("arms"), [1])).
//     toEqual(new T.App(pants, new T.Ref("arms")))
// })

// it("edit expr: let", () => {
//   const pants = "pants"
//   const slacks = new T.Ref("slacks")
//   const outfit = new T.Ref("outfit")
//   const orig = new T.Let(pants, T.notype, slacks, outfit)
//   expect(orig.editName(old => "bottoms",  [0])).
//     toEqual(new T.Let("bottoms", T.notype, slacks, outfit))
//   expect(orig.editExpr(old => new T.Ref("jeans"), [2])).
//     toEqual(new T.Let(pants, T.notype, new T.Ref("jeans"), outfit))
//   expect(orig.editExpr(old => new T.Ref("suitcase"), [3])).
//     toEqual(new T.Let(pants, T.notype, slacks, new T.Ref("suitcase")))
// })
