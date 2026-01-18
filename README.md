A sandbox 2D physics kinematics engine that simulates gravity, momentum, and collisions in real-time. Built with OCaml using the Core and Async libraries, with graphics rendering via OCaml Graphics.

The engine uses a timestep system that advances the simulation in discrete steps (default: 0.001s). Each timestep processes collision reactions, updates object velocities based on forces, and then updates positions. User interactions are handled through event-driven input listeners that asynchronously wait for mouse clicks and button events, allowing seamless object creation, dragging, and UI control without blocking the physics simulation loop.

**Core Physics**
- `vector.mli` - 2D vector operations and transformations
- `units.mli` - conversions between pixel coordinates and physics units
- `objects.mli` - object types (Ball, Box, Line, Cup) with position, velocity, and physics properties
- `force.mli` - force representation for applying physics effects to objects
- `collisions.mli` - collision detection between objects (ball-ball, ball-line, ball-cup, etc.)
- `reactions.mli` - collision response handling and reaction effects
- `step.mli` - physics simulation time step execution

**User Interface**
- `click_state.mli` - tracks user interaction state (creating objects, dragging, selecting)
- `click_interactions.mli` - handles mouse click events and user input
- `panel.mli` - UI components (buttons and text boxes)
- `canvas.mli` - bounded area for managing and rendering physics objects
- `interface.mli` - combines panel and canvas into a unified interface

**Simulation Runtime**
- `world_state.mli` - simulation status tracking (In_progress, Paused, Clear, Failure)
- `world_graphics.mli` - graphics initialization and rendering system
- `world.mli` - main world container combining state, interface, and interaction handling
- `run.mli` - main entry point that starts the simulation

![Screenshot 1](images/Screenshot%202026-01-14%20233226.png)

![Screenshot 2](images/Screenshot%202026-01-14%20233332.png)

![Screenshot 3](images/Screenshot%202026-01-14%20233447.png)

![Screenshot 4](images/Screenshot%202026-01-14%20233530.png)

![Screenshot 5](images/Screenshot%202026-01-14%20233931.png)

![Screenshot 6](images/Screenshot%202026-01-14%20234840.png)
