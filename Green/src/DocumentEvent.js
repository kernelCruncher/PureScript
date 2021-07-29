'use strict';

exports.clickEvent = 'click';

exports.inputEvent = 'input';

exports.addEvent = function (eventName) {
    return function (eventHandler) {
      return function () {
        return document.addEventListener(eventName, (evt) => {
          return eventHandler(evt)();
        })
      }
    }
  }

