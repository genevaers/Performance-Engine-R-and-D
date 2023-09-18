# Performance-Engine-Extensions

These extensions contain sample GenevaERS user exits as well as utilities useful to running and testing the Performance Engine.

## GVBDTGEN

Generator for sample test data used in conjunction with GVBDEMO views.

GVBDTGEN -- ASM source code of data generator.

DTGENASM -- JCL to assemble data generator.

DTGENLNK -- JCL to link data generator.

DTGENRUN -- JCL to execute data generator.

## Example GenevaERS Performance Engine Exits

GVBXRCK  -- read exit implementing common key buffering.

GVBXLCK  -- related lookup exit implementing common key buffering.

GVBXWCK  -- related write exit implementing common key buffering.


GVBXR6   -- COBOL read exit using QSAM.

GVBXR7   -- ASM read exit using QSAM.

GVBXR8   -- COBOL read exit using VSAM.

GVBXR9   -- ASM read exit using VSAM.


GVBXR1   -- ASM read exit using ICHEINTY to obtain RACF definitions of users.

GVBXR3   -- ASM read exit using ICHEINTY to obtain RACF definitions of groups.

GVBXR5   -- ASM read exit used to correlate these results and produce report.


