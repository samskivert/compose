import * as React from 'react';
import * as ReactDOM from 'react-dom';

import * as T from './trees'
import * as W from './workspace'

const store = new W.WorkspaceStore()
store.addDef(T.listExample)
store.addDef(T.consFunExample)
store.addDef(T.aliasExample)
store.addDef(T.recordExample)
store.addDef(T.boxExample)
store.addDef(T.fibExample)
store.addDef(T.revExample)
store.selidx = 0

ReactDOM.render(<W.Workspace store={store} />, document.getElementById('root'));
