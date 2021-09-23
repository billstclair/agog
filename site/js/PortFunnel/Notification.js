//////////////////////////////////////////////////////////////////////
//
// Notification.js
// JavaScript runtime code for Elm Notification module.
// Copyright (c) 2018-2021 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE.txt
//
//////////////////////////////////////////////////////////////////////

(function(scope) {
  var moduleName = 'Notification';
  var sub;

  var isAvailable =
      window.Notification != null;

  var id = 0;
  var notifications = [];  

  function init() {
    var PortFunnel = scope.PortFunnel;
    if (!PortFunnel || !PortFunnel.sub || !PortFunnel.modules) {
      // Loop until PortFunnel.js has initialized itself.
      setTimeout(init, 10);
      return;
    }
    
    sub = PortFunnel.sub;
    PortFunnel.modules[moduleName] = { cmd: dispatcher };

    // Let the Elm code know we've started, with an IsAvailableAnswer message
    sub.send({ module: moduleName,
               tag: "wasAvailable",
               args : isAvailable
             });
  }

  init();

  function returnError(msg) {
      return { moduleName: moduleName,
               tag: 'error',
               args: msg
             }
  }

  function ifAvailable(res) {
      if (isAvailable) {
          res.module = moduleName;
          return res;
      } else {
            returnError('Notifications are not available.')
      }
  }

  function dispatcher(tag, args) {
    if (tag == 'isAvailable') {
      return { module: moduleName,
               tag: 'wasAvailable',
               args: isAvailable
             };
    } else if (tag == 'getPermission') {
        return ifAvailable({ tag: 'gotPermission',
                             args: Notification
                           });
    } else if (tag == 'requestPermission') {
        // This is the old callback version. More compatible.
        Notification.requestPermission(function(res) {
            sub.send({ module: moduleName,
                       tag: 'gotPermission',
                       args: res
                     });
        });
    } else if (tag == 'sendNotification') {
        var title = args;
        if (isAvailable) {
            notification = new Notification(title);
            newid = id++;
            notifications[newid] = notification;
            notification.onclose = function() { delete notifications[newid] };
            return ifAvailable({ tag: 'notification',
                                 args: { id: newid, title: title }
                               });
        }
        else {
            ifAvailable({});
        }
    } else if (tag == 'dismissNotification') {
        var myid = args;
        var notification = notifications[myid];
        if (notification) {
            notification.close();
            delete notifications[myid];
        } else
            return returnError('Notification does not exist: ' + myid);
    } else if (tag == 'lookupNotification') {
        var myid = args;
        var notification = notifications[myid];
        if (notification) {
            ifAvailable({ tag: 'notification',
                          args: { id: id, title: notification.title }
                        });
        } else {
            return returnError('Notification does not exist: ' + myid);
        }
    }
  }

        
})(this);   // Execute the enclosing function
