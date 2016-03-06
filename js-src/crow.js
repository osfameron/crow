import Rx from 'rx';
import Cycle from '@cycle/core';
import {makeDOMDriver, hJSX, isolateSink} from '@cycle/dom';

function main(drivers) {

    const toggleStartState = false

    const clicksApp$ = drivers.DOM.select('input').events('click')
      .map(ev => ev.target.checked)
      .startWith(toggleStartState)

    const clicksApp2$ = drivers.ViewDOM.select('input').events('click')
      .map(ev => ev.target.checked)
      .startWith(toggleStartState)

    const toggled$ = Rx.Observable.merge(clicksApp$, clicksApp2$)

    return {
        DOM: toggled$.map(toggled =>
            <div>
              <input type="checkbox" /> Toggle me
              <p>{ toggled ? 'ON' : 'OFF'}</p>
            </div>
        ),
        WriteDOM: toggled$
    };
}

function addClassToInput(focus, [, toggled]) {
    const inputs = focus.querySelectorAll('input');

    for (var i=0; i < inputs.length; i++) {
        const x = inputs[i];
        if (toggled) {
            x.classList.add('foo')
        }
        else {
            x.classList.remove('foo')
        }
    }
    return {}
}

function makeWriteDOMDriver(mount, f) {
    const focus = document.querySelector(mount);
    return stream$ => {
        const first = stream$.first()
        stream$.startWith([first, first])
            .scan(([, oldv], newv) => [oldv, newv])
            .subscribe( datum => f(focus, datum) );
    };
}

window.onload = () => {
    const drivers = {
      DOM: makeDOMDriver('#app'),
      ViewDOM: makeDOMDriver('#app2'),
      WriteDOM: makeWriteDOMDriver('#app2', addClassToInput)
    };
    Cycle.run(main, drivers)
};
