import { observable } from 'mobx'
import { UUID, Module } from './module'

export class Project {

  /** The human readable name of this project. */
  @observable name :string

  /** A URL that identifies the DVCS source of this project. */
  @observable source :string

  /** The components that make up this project. */
  @observable components :Component[] = observable.array([])

  constructor (readonly uuid :UUID, name :string, source :string) {
    this.name = name
    this.source = source
  }
}

/** Components define either libraries or applications. */
export enum Type { LIB, APP }

/** A dependency on one or more components of a project. */
export type Depend = {
  source :string
  puuid :UUID
  cuuids :UUID[]
}

export class Component {

  /** The human readable name of this component. */
  @observable name :string

  /** The modules that make up this component. */
  @observable modules :Module[] = observable.array([])

  /** The project/components on which this component depends. */
  @observable depends :Depend[] = observable.array([])

  constructor (readonly uuid :UUID, readonly type :Type, name :string) {
    this.name = name
  }
}
