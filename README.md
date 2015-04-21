ACO-DDVO and ACO-SDVO are two tools coded by FORTRAN to support for optimization of irrigation scheduling. They represent the irrigation scheduling problem in the form of a decision tree and use ant colony optimization (ACO) as the optimization engine. While ACO-DDVO can dynamically adjust decision variable options during the optimization process, ACO-SDVO uses static decision variable options and penalty functions. Therefore, ACO-DDVO consistently outperforms ACO-SDVO in terms of solution quality and computational efficiency.

Two formulations are considered for ACO-DDVO and ACO-SDVO:

- Formulation 1: There is only one decision variable, i.e. crop type. The software for Formulation 1 is stored in the branch "ACO-DDVO_Formulation-1" and "ACO-SDVO_Formulation-1".

- Formulation 2: There are two decision variables, including crop type and the depth of irrigation water. The software for Formulation 2 is stored in the branches of "ACO-DDVO_Formulation-2" and "ACO-SDVO_Formulation-2".
 
The input data for both formulations are stored in the branch "Input_Data".
