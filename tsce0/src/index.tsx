import * as React from 'react';
import * as ReactDOM from 'react-dom';

import * as W from './workspace'
import * as P from './prefab'

const store = new W.WorkspaceStore()
store.projects.push(P.primProject)
store.projects.push(P.mkTestProject(store, store))
store.selprojidx = 1

// TEMP: open all the defs in our test module
const comp0 = store.selectedComponent
const mod0 = comp0 && comp0.modules[0]
if (mod0) mod0.defs.forEach(d => store.openDef(d))

ReactDOM.render(<W.Workspace store={store} />, document.getElementById('root'));
