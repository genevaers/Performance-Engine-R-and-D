# Performance-Engine-Extensions

These extensions contain sample GenevaERS user exits as well as utilities useful to running and testing the Performance Engine.

## GVBDTGEN

Generator for sample test data used in conjunction with GVBDEMO views

GVBDTGEN -- ASM source code of data generator

DTGENASM -- JCL to assemble data generator

DTGENLNK -- JCL to link data generator

DTGENRUN -- JCL to execute data generator

## Example GenevaERS Performance Engine Exits

GVBXRCK  -- read exit implementing common key buffering

GVBXLCK  -- related lookup exit implementing common key buffering

GVBXWCK  -- related write exit implementing common key buffering


GVBXLENV -- lookup exit to obtain environment information

GVBXLFIL -- lookup exit to obtain file information

GVBXLST  -- lookup exit to concatenate list of strings

GVBXLUDB -- lookup exit which reads DB2 table by key value


GVBXR6   -- COBOL read exit using QSAM (see BLDXR6 to build, DEMORXIT for run job)

GVBXR7   -- ASM read exit using QSAM (see BLDXR7 to build, DEMORXIT for run job)

GVBXR8   -- COBOL read exit using VSAM (see BLDXR8 to build, DEMORXIT for run job)

GVBXR9   -- ASM read exit using VSAM (see BLDXR9 to build, DEMORXIT for run job)

GVBXRA   -- ASM read exit to read ADABAS data


GVBXR1   -- ASM read exit using ICHEINTY to obtain RACF definitions of users (see BLDXR1 for build job)

GVBXR3   -- ASM read exit using ICHEINTY to obtain RACF definitions of groups (see BLDXR3 for build job)

GVBXR5   -- ASM read exit used to correlate both above results and produce report (BLDXR5 for build job)

The JCL to run Performance Engine with these exits is: DEMOXD1, DEMOXD2, DEMOXD3, DEMOXD4, DEMOXD1, DEMOXD2, DEMOXD3, DEMOXD4


## Example utility routines available for GenevaERS Performance Engine Exits

GVBJDAT  -- ASM date routine

GVBJDAY  -- ASM julian calendar routine

GVBMR94  -- ASM wait/delay routine

GVBRD15  -- ASM routine to build Geneva reference data file

GVBRD35  -- ASM routine to build Geneva reference data file

GVBUR05  -- ASM routine to allocate ZOS getmain storage (for old COBOL)

GVBUR10  -- ASM routine to release ZOS getmain storage (for old COBOL)

GVBUR15  -- ASM routine to set pointer variable to address of data area (for old COBOL)

GVBUR30  -- ASM routine to read DB2 table

GVBUR40  -- ASM routine to compress/decompress a record

GVBUR45  -- ASM routine to initialize Geneva memory resident lookup table

GVBUR66  -- ASM routine for ENQ/DEQ (for COBOL)

GVBXP01  -- ASM routine to return address of logical record for a given key.

## GenevaERS utlities

GVBMR93  -- ASM program used to schedule execution of multiple batch jobs in specified sequence

GVBUDIR  -- ASM program used to obtain list of PDS(E) members (see PDSSRCHR to run)

GVBUPDS  -- ASM program used to scan list of PDS(E) members for certain characters (see PDSSRCHR to run).

DEFVCUST -- JCL to define customer file as VSAM cluster

LSTVSAM  -- JCL to list/delete customer file VSAM cluster

MLOADVS  -- COBOL program to load customer file VSAM cluster (see BLDLOAD to build, RUNLOAD for run job)

MBRSEVS  -- COBOL program to read customer file VSAM cluster (see BLDBRWS to build, RUNBRSE for run job)





