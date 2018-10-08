import * as React from 'react';
import * as ReactDOM from 'react-dom';

import * as W from './workspace'
import * as P from './prefab'
import * as S from './store'

const store = new S.LocalStore()
P.seedTestProject(store)

const wspace = new W.WorkspaceStore(store)
wspace.projects.push(P.primProject)
wspace.openProject(P.testProjUUID)
wspace.selprojidx = 1

// TEMP: open all the defs in our test module
const comp0 = wspace.selectedComponent
const mod0 = comp0 && comp0.modules[0]
if (mod0) mod0.defs.forEach(d => wspace.openDef(d))

ReactDOM.render(<W.Workspace store={wspace} />, document.getElementById('root'));
