turtles-own [
  sugar           ;; the amount of sugar this turtle has
  metabolism      ;; the amount of sugar that each turtles loses each tick
  vision          ;; the distance that this turtle can see in the horizontal and vertical directions
  vision-points   ;; the points that this turtle can see in relative to it's current position (based on vision)
]

patches-own [
  psugar           ;; the amount of sugar on this patch
  max-psugar       ;; the maximum amount of sugar that can be on this patch
  psugar-increment ;; the amount of sugar the patch regrows per unit of time
]

;;
;; Setup Procedures
;;

to setup
  clear-all
  create-turtles initial-population [ turtle-setup ]
  setup-patches
  update-plots
end

to turtle-setup ;; turtle procedure
  set color red
  set shape "circle"
  move-to one-of patches with [(pxcor < 18) and (pycor < 18) and (not any? other turtles-here)]
  set sugar random-in-range 5 25
  set metabolism random-in-range 1 4
  set vision random-in-range 30 60 
  ;; turtles can look horizontally and vertically up to vision patches
  ;; but cannot look diagonally at all
  set vision-points []
  foreach n-values vision [? + 1]
  [
    set vision-points sentence vision-points (list (list 0 ?) (list ? 0) (list 0 (- ?)) (list (- ?) 0))
  ]
  run visualization
end

to setup-patches
  file-open "sugar-map.txt"
  foreach sort patches
  [
    ask ?
    [
      set max-psugar file-read
      set psugar max-psugar
      patch-recolor
    ]
  ]
  file-close
end

;;
;; Runtime Procedures
;;

to go
  if not any? turtles [
    stop
  ]
  ask patches [
    patch-growback
    patch-recolor
  ]
  ask turtles [
    turtle-move
    turtle-eat
    if sugar <= 0
      [ die ]
    run visualization
  ]
  tick
  update-plots
end

to turtle-move ;; turtle procedure
  ;; consider moving to unoccupied patches in our vision, as well as staying at the current patch
  let move-candidates (patch-set patch-here (patches at-points vision-points) with [not any? turtles-here])
  ;show move-candidates
  ;let test (patch-set patch 0 0 patch 1 3 patch 4 -2)
  ;show test
  let possible-winners move-candidates with-max [psugar]
  ;show possible-winners
  if any? possible-winners [
    ;; if there are any such patches move to one of the patches that is closest
    move-to min-one-of possible-winners [distance myself]
  ]
end

to turtle-eat ;; turtle procedure
  ;; metabolize some sugar, and eat all the sugar on the current patch
  set sugar (sugar - metabolism + psugar)
  set psugar 0
end

to patch-recolor ;; patch procedure
  ;; color patches based on the amount of sugar they have
  set pcolor (yellow + 4.9 - psugar)
end

to patch-growback ;; patch procedure
  ;; grow back one unit of sugar per time period
  if psugar < max-psugar [set psugar (psugar + 1)]   
end

;;
;; Plotting Procedures
;;

to update-plots
  set-current-plot "Population"
  plotxy ticks count turtles
  set-current-plot "Wealth distribution"
  set-histogram-num-bars 10
  set-plot-x-range 0 (max [sugar] of turtles)
  set-plot-pen-interval ((max [sugar] of turtles) / 10)
  histogram [sugar] of turtles
  set-current-plot "Average vision"
  plotxy ticks mean [vision] of turtles
  set-current-plot "Average metabolism"
  plotxy ticks mean [metabolism] of turtles
end

;;
;; Utilities
;;

to-report random-in-range [low high]
  report low + random (high - low + 1)
end

;;
;; Visualization Procedures
;;

to no-visualization ;; turtle procedure
  set color red
end

to color-agents-by-vision ;; turtle procedure
  set color red - (vision - 3.5)
end

to color-agents-by-metabolism ;; turtle procedure
  set color red + (metabolism - 2.5)
end


; Copyright 2011 Roberto Ferrer. All rights reserved.
; The full copyright notice is in the Information tab.
@#$#@#$#@
GRAPHICS-WINDOW
300
10
710
441
-1
-1
8.0
1
10
1
1
1
0
1
1
1
0
49
0
49
1
1
1
ticks

BUTTON
10
55
90
95
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
100
55
190
95
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
200
55
290
95
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

CHOOSER
10
105
290
150
visualization
visualization
"no-visualization" "color-agents-by-vision" "color-agents-by-metabolism"
0

PLOT
720
10
940
165
Population
NIL
NIL
0.0
10.0
0.0
10.0
true
false
PENS
"default" 1.0 0 -16777216 true

PLOT
950
10
1170
165
Wealth distribution
NIL
NIL
0.0
10.0
0.0
10.0
true
false
PENS
"default" 1.0 1 -16777216 true

SLIDER
10
15
290
48
initial-population
initial-population
10
1000
250
10
1
NIL
HORIZONTAL

PLOT
720
175
940
330
Average vision
NIL
NIL
0.0
10.0
0.0
6.0
true
false
PENS
"default" 1.0 0 -16777216 true

PLOT
950
175
1170
330
Average metabolism
NIL
NIL
0.0
10.0
0.0
5.0
true
false
PENS
"default" 1.0 0 -16777216 true

MONITOR
95
160
190
209
population
count turtles
17
1
12

@#$#@#$#@
WHAT IS IT?
-----------
This model implements Epstein & Axtell's Sugarscape model under rules ({G1},{M}) as described in chapter 2 of their book Growing Artificial Societies: Social Science from the Bottom Up. It simulates a population with limited, spatially-distributed resources available. The main purpose of this model is to show how emergent diagonal waves of migrators can form from an initial distribution of agents in a block. Otherwise, it is the same as Sugarscape 2 [G1, M].

HOW IT WORKS
------------
Each patch contains some sugar, the maximum amount of which is predetermined. At each tick, each patch grows back one unit of sugar up to the maximum predetermined amount. The amount of sugar a patch currently contains is indicated by its color; the darker the yellow, the more sugar.

At setup, agents are placed in a block form situated a one "corner" of the world. Each agent can only see a certain distance horizontally and vertically. At each tick, each agent will move to the nearest unoccupied location within their vision range with the most sugar, and collect all the sugar there.  If its current location has as much or more sugar than any unoccupied location it can see, it will stay put.

Agents also use (and thus lose) a certain amount of sugar each tick, based on their metabolism rates. If an agent runs out of sugar, it dies.

HOW TO USE IT
-------------
Set the INITIAL-POPULATION slider before pressing SETUP. This determines the number of agents in the world.

Press SETUP to populate the world with agents and import the sugar map data. GO will run the simulation continuously, while GO ONCE will run one tick.

The VISUALIZATION chooser gives different visualization options and may be changed while the GO button is pressed. When NO-VISUALIZATION is selected all the agents will be red. When COLOR-AGENTS-BY-VISION is selected the agents with the longest vision will be darkest and, similarly, when COLOR-AGENTS-BY-METABOLISM is selected the agents with the lowest metabolism will be darkest.

The four plots show the world population over time, the distribution of sugar among the agents, the mean vision of all surviving agents over time, and the mean metabolism of all surviving agents over time.

THINGS TO NOTICE
----------------
Check the continuous buzz of activity. Agents concentrate their activities on the sugar peaks where two colonies seem to form.

A small number of agents at the borders of the Sugarscape can be seen to not move at all. This is because the agents have reached places in the world where they can no longer see better unoccupied locations near them. Since one unit of sugar grows back each tick, these agents to be able to survive must i) have a great stock of sugar holdings that will deplete faster or slower depending on its metabolism rate or ii) have the minimum metabolism rate (= 1) which is the same as the sugar growback rule. Agents that do not meet condition (ii) will evetually die. 

Because agents live forever, no stationary wealth distribution ever obtains. The agents simply accumulate indefinitely.

Recall individual agents are able to move only north, south, east and west. Yet the collective wave is clearly moving northeast. The local rule precludes individual beahavior mimicking the collective behavior. This is an example of emergence. (Epstein & Axtell indicate that a random vision between 1 and 10 for each agent is enough to provoque this behavior. However, we needed to increment those amounts --10 and 60-- to get it.) 

THINGS TO TRY
-------------
Try varying the initial POPULATION. What effect does the initial POPULATION have on the final stable population? Does it have an effect on the distribution of agent properties, such as vision and metabolism?

NETLOGO FEATURES
----------------
All of the Sugarscape models create the world by using FILE-READ to import data from an external file, "sugar-map.txt". This file defines both the initial and the maximum sugar value for each patch in the world.

Since agents cannot see diagonally we cannot use IN-RADIUS to find the patches in the agents' vision.  Instead, we use AT-POINTS.

CREDITS AND REFERENCES
----------------------
Epstein, J. and Axtell, R. (1996). Growing Artificial Societies: Social Science from the Bottom Up.  Washington, D.C.: Brookings Institution Press.

HOW TO CITE
-----------
If you mention this model in an academic publication, we ask that you include these citations for the model itself and for the NetLogo software:
- Ferrer, R. (2011). NetLogo Sugarscape 5 Migration Model. Modeling the Social Sciences at http://hardsciences.wordpress.com  
- Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

COPYRIGHT NOTICE
----------------
Copyright 2011 Roberto Ferrer. All rights reserved.

Permission to use, modify or redistribute this model is hereby granted, provided that both of the following requirements are followed:
a) this copyright notice is included.
b) this model will not be redistributed for profit without permission from Roberto Ferrer. Contact refp16 AT gmail DOT COM for appropriate licenses for redistribution for profit.

This model is based on:
- Li, J. and Wilensky, U. (2009).  NetLogo Sugarscape 1 Immediate Growback model.  http://ccl.northwestern.edu/netlogo/models/Sugarscape1ImmediateGrowback.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL. 
Copyright 2009 Uri Wilensky. All rights reserved.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 4.1.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
1
@#$#@#$#@
