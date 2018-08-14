import * as React from 'react';
import * as ReactDOM from 'react-dom';

import * as W from './workspace'
import * as P from './prefab'

const store = new W.WorkspaceStore()
// store.addDef(T.fibExample)
store.addDef(P.listExample)
// store.addDef(T.consFunExample)
// store.addDef(T.aliasExample)
store.addDef(P.recordExample)
store.addDef(P.boxExample)
// store.addDef(T.revExample)
store.selidx = 0

ReactDOM.render(<W.Workspace store={store} />, document.getElementById('root'));
