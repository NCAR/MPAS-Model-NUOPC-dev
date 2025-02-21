# MPAS-Hydro README

A document for the planning and implementation of coupling WRF-Hydro with MPAS.
The coupling mechanism will use [Earth System Modeling Framework](https://earthsystemmodeling.org/)
(ESMF) and the [National Unified Operational Prediction Capability](https://earthsystemmodeling.org/nuopc)
(NUOPC) interoperability layer, also revered to as a cap.

## Goals
- NUOPC caps
  - two-way interfacing: WRF-Hydro runs the NoahMP land model
  - one-way interfacing: WRF-Hydro passes routing data
- Testcases

## MPAS and MPAS-Hydro Build Process
- [ ] add WRF-Hydro as submodule


## NUOPC Cap Exchange
- [ ] Add list of variables being exchanged for two and one way coupling

## Graphs
- [ ] create graph of procedure


## Directory Structure
```
mpas/
├──src/
│   ├──x_hydro/
│   ├──core_atmosphere/
│   ├──core_init_atmosphere/
│   ├──core_landice/
│   ├──core_ocean/
│   ├──core_seaice/
│   ├──core_sw/
│   ├──core_test/
│   ├──driver/
│   ├──external/
│   ├──framework/
│   ├──operators/
│   └──tools/
└──testing_and_setup/
```
