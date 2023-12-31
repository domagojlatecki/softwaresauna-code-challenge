# Software Sauna Code Challenge Solution

Solution for the [Software Sauna Code Challenge](https://github.com/softwaresauna/code-challenge).

The structure and various tasks of this project are defined in `build.sc`.

Targeted Java version for the build: `17`

## Project Import via BSP/Bloop

To generate or refresh project BSP model, run `./bsp.sh` script.  
To export project via Bloop, run `./bloop.sh` script.

## Build

To compile the project, run: `./mill compile`.  
To reformat code to conform with style defined in `.scalafmt.conf`, run: `./mill reformat`.  
To execute tests, run: `./mill test`.  
To execute only unit tests, run: `./mill unitTest`.  
To execute only integration tests, run: `./mill integTest`.  
To execute only api tests, run: `./mill apiTest`.  
To execute tests with code coverage, run `./mill coverage` or alternatively execute the `coverage.sh` script.  

To run the compiled application, run: `./mill app.run <app args>`  

Other commands can be found in `build.sc` file.
