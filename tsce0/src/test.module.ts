import { UUID } from "./names"
import * as P from "./prefab"
import * as S from "./symbols"
import * as M from "./module"

const modResolver :M.Resolver = {
  resolveModule :(uuid :UUID) => P.primProject.components[0].modules[0]
}
const testProj = P.mkTestProject({
  resolveProject: (uuid :UUID) => P.primProject
}, modResolver)
const testMod = testProj.components[0].modules[0]

it("deflate/inflate/deflate round trip", () => {
  const modJson = testMod.deflate()
  const modJsonStr = JSON.stringify(modJson, undefined, " ")

  const infMod = M.inflateMod(S.emptyScope, modResolver, modJson)
  const infModJson = infMod.deflate()
  const infModJsonStr = JSON.stringify(infModJson, undefined, " ")
  expect(infModJsonStr).toEqual(modJsonStr)
})
