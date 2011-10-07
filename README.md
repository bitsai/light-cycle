A simple Tron light cycle game in Clojure, inspired by the Snake example in [Programming Clojure](http://pragprog.com/titles/shcloj/programming-clojure) and [Google's Tron AI Challenge](http://csclub.uwaterloo.ca/contest/).

Current gameplay is Free-For-All between 2 humans and 2 bots:
* Player 1 controls the yellow light cycle using WASD.
* Player 2 controls the blue light cycle using the arrow keys.
* Right now, the bots are not very smart.  They simply choose a random open adjacent square, with a bias for going straight if possible.

To start the game, run light_cycle.clj:
    clj light_cycle.clj
