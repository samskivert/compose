import * as React from 'react';
import * as ReactDOM from 'react-dom';

import * as W from './workspace'
import * as P from './prefab'

const store = new W.WorkspaceStore()
store.projects.push(P.primProject)
store.projects.push(P.mkTestProject(store))
store.selprojidx = 1

ReactDOM.render(<W.Workspace store={store} />, document.getElementById('root'));
