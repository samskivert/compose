import * as React from 'react';
import * as ReactDOM from 'react-dom';
// import {observable} from 'mobx';
// import {observer} from 'mobx-react';
// import DevTools from 'mobx-react-devtools';

import * as T from './trees'
import * as E from './editor'

const store = new E.DefStore(T.revExample)
ReactDOM.render(<E.DefEditor store={store} />, document.getElementById('root'));
