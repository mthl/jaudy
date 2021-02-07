# jaudy

A minimalistic, flexible and performant data oriented URI path router for Clojure and ClojureScript.

The design is inspired by [Pedestal](http://pedestal.io) router and the implementation is based on the highly performant [Reitit](https://cljdoc.org/d/metosin/reitit) router.

## Usage

```
(require '[jaudy.route :as r])

(def routes
 [{:route/path "/foo"
   :route/id ::foo}
  {:route/path "/bar/{x}"
   :route/id ::bar}])

(def router (r/router routes))

(r/match router "/foo")
;;; ⤷ {:data {:route/path "/foo" :route/id ::foo} :params {}}

(r/path-for router ::bar {:x "baz"})
;;; ⤷ "/bar/baz"
```

## License

Copyright © 2017-2021  Metosin Oy
Copyright © 2021  Mathieu Lirzin

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
