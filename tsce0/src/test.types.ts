import * as TP from './types'
import * as C from './constants'

it('joins things', () => {
  const scalar = new TP.Scalar(C.Tag.Int, 32)
  expect(scalar.join(TP.hole).isError).toEqual(false)
  // console.log(scalar.join(TP.hole))
})
