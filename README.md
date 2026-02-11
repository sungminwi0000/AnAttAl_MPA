# AnAttAl
### Automated local simulation workflow about resistance spot welding for the AnAttAl project (MPA University of Stuttgart)

#### Simulation Concept

The simulation is organized as a coupled thermo–electro–mechanical workflow.
First, Abaqus is used to build the FE model and generate the `.inp` file containing geometry, mesh, elements, and material definitions. The `.inp` is then read by a `Fortran-based` solver that performs the coupled computation.

Three measured welding signals are used as inputs: `force`, `current`, and `voltage`. The force drives the mechanical part, while current and voltage are used in a resistance submodel to compute the time-dependent `electrical resistance`. This resistance is iteratively used to update key electrical parameters (e.g., effective conductivity/contact behavior), enabling a realistic representation of resistance spot welding and the resulting temperature and mechanical response.

<img width="1826" height="746" alt="RSW_Pyiron_workflow" src="https://github.com/user-attachments/assets/7cbeaa28-a6fb-4905-920b-2f14624dfa7e" />


#### Repository Structure
In addition to the Pyiron-based workflow definition, this repository contains all scripts and source files required to execute the complete simulation pipeline:

##### Abaqus Scripts
- `abaqusMacros.py`

  Abaqus macro script for geometry generation and preprocessing.
  Used to create the FE model and export the .inp file.

##### .inp Processing Script
- `File_Programm2.py`

  Python script for parsing the generated .inp file and extracting required model data.

##### Welding Process Data (FAMOS)
- `Stromverlauf.txt`
- `Kraftverlauf.txt`
- `Potentialverlauf.txt`

  Measured welding signals (current, force, voltage) used as input for the coupled simulation.

##### Fortran Simulation Codes
- `Testfall_1ms_Dyn5_a_a4.for`

  Main thermo-electro-mechanical simulation solver.
- `odbjoin.for`

  Post-processing tool for merging restart .odb files.

##### Compiler Environment
- `Compiler 16.0 Update 1 for Intel 64 Visual Studio 2015 environment.lnk`

  Windows shortcut used to activate the Intel Fortran compiler environment.


#### Pyiron workflow
The Pyiron workflow orchestrates these components:

- Geometry modification (Abaqus macro-based)

- Abaqus preprocessing and `.inp` generation

- Data extraction from `.inp` files

- Import of welding process data (FAMOS)

- Fortran-based thermo-electro-mechanical simulation

- Post-processing and consolidation of `.odb` files

The workflow is implemented using PyironFlow, enabling structured execution and dependency management across all simulation stages.

