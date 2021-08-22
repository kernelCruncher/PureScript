# PureScript
There are several PureScript projects in the repo. The key files are index.html and index.js. To run the application open the index.html file in a browser.

+ The first project "Random" is an extension of an exercise in the _PureScript by Example_ book. Clicking on the canvas causes 100 circles of random sizes, colour and location to appear on the canvas. As the user continues to clic,k the circles begin to layer in a rather psychedelic way.
+ The second project "Fractal" is another extension of an exercise in the _PureScript by Example_ book. Here, a user clicks on a canvas and a fractal gradually emerges.
+ The third project "Monad Adventure" is another extension of an exercise in the _PureScript by Example_ book. Here, the user can play a game through the Console and pick up various objects, such as Matches and Candles. A cheat mode was added, and the code was rewritten using the ExceptT tarnsformer so that errors were not included with informational instructions in the Writer list of the RWS monad.
+ The fourth project, "Green", is just code from an online tutorial. The functionality is quite simple: the user inputs text into a textbox and sees the same text appear above the input. This employs some more advanced concepts, like the Foreign Function Interface.
+ The ClickGame is a work in progress. It's an attempt to create a Click Game such that the user has to match the Canvas elements that have the same colour. I'm in the process of coding it without using any UI reactive frameworks.
+ The ClickGameHalogen is the Click game described above but with Buttons instead of Canvas elements and using the Halogen Reactive Framework to capture the state of the components.
