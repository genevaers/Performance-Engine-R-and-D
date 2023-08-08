        TITLE 'GVBXR5 - READ EXIT - MERGE USER/GROUP PERMISSIONS'               
**********************************************************************          
*                                                                               
* (C) COPYRIGHT IBM CORPORATION 2023.                                     
*     Copyright Contributors to the GenevaERS Project.                          
* SPDX-License-Identifier: Apache-2.0                                           
*                                                                               
**********************************************************************          
*                                                                               
*  Licensed under the Apache License, Version 2.0 (the "License");              
*  you may not use this file except in compliance with the License.             
*  You may obtain a copy of the License at                                      
*                                                                               
*     http://www.apache.org/licenses/LICENSE-2.0                                
*                                                                               
*  Unless required by applicable law or agreed to in writing, software          
*  distributed under the License is distributed on an "AS IS" BASIS,            
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express                 
*  or implied.                                                                  
*  See the License for the specific language governing permissions              
*  and limitations under the License.                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
***********************************************************************         
*                                                                     *         
*  MODULE DESCRIPTION     : THIS MODULE WORKS WITHIN THE "GENEVA"     *         
*                           ENVIRONMENT AND IS A READ EXIT.           *         
*                                                                     *         
*                         : GVBXR1 GVBXR3 and GVBXR5 read RACF DSN    *
*                           profiles via ICHEINTY. Geneva decides the *
*                           dataset profiles it is interested in, by  *
*                           using a VIEW containing lookup by USERID. *
*                           A list of RACF GROUPS is built to cross   *
*                           reference USERID with GROUP membership.   *
*                           See VIEW Demo.Open.Source.Exit.Views.xml  *
*                           APF authorization is required.            *
*                                                                     *         
***********************************************************************         
                        EJECT                                                   
***********************************************************************         
*                                                                     *         
*           MODULE RETURN CODES AND REGISTER DOCUMENTATION            *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
*  RETURN CODES:                                                      *         
*                                                                     *         
*          0   - SUCCESSFUL                                           *         
*          4   -                                                      *         
*          8   - END-OF-FILE                                          *         
*         12   - DISABLE   VIEW                                       *         
*         16   - ABEND     JOB                                        *         
*                                                                     *         
*                                                                     *         
*  REGISTER USAGE:                                                    *         
*                                                                     *         
*        R15 - TEMPORARY WORK REGISTER                                *         
*            - RETURN    CODE                                         *         
*                                                                     *         
*        R14 - TEMPORARY WORK REGISTER                                *         
*            - RETURN    ADDRESS                                      *         
*                                                                     *         
*        R13 - REGISTER  SAVE AREA    ADDRESS (GVBXR5  WORK AREA)     *         
*                                                                     *         
*        R12 - PROGRAM   BASE REGISTER                                *         
*        R11 - Work register                                          *         
*                                                                     *         
*        R10 - INTERNAL  SUBROUTINE   RETURN   ADDRESS                *         
*                                                                     *         
*        R9  - PARAMETER LIST GENPARM ADDRESS                         *         
*                                                                     *         
*        R8  - WORK   REGISTER                                        *         
*                                                                     *         
*        R7  - CURRENT   EVENT FILE   RECORD  ADDRESS                 *         
*        R6  - CURRENT   FILE  CONTROL BLOCK  ADDRESS                 *         
*        R5  - CURRENT   OPEN  FILE     LIST  ENTRY                   *         
*                                                                     *         
*        R4  - STARTING  FILE  CONTROL BLOCK  ADDRESS                 *         
*        R3  - STARTING  OPEN  FILE     LIST  ENTRY    (BREAK)        *         
*                                                                     *         
*        R2  - LOWEST    KEY   VALUE   EVENT  RECORD                  *         
*        R1  - LOWEST    KEY   VALUE    FILE  AREA     ADDRESS        *         
*            - PARAMETER LIST  ADDRESS                                *         
*            - TEMPORARY WORK  REGISTER                               *         
*                                                                     *         
*        R0  - TEMPORARY WORK  REGISTER                               *         
*                                                                     *         
***********************************************************************         
*                                                                               
                        EJECT                                                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*        GENEVA PROVIDED PARAMETER LISTS                              *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         COPY  GVBX95PA         GENEVA  PARAMETER LIST                          
         copy  gvbhdr                                                           
                                                                                
         IHASAVER DSECT=YES,SAVER=YES,SAVF4SA=YES,SAVF5SA=YES,TITLE=NO          
*                                                                               
***********************************************************************         
*                                                                     *         
*                     W O R K   A R E A                               *         
*                                                                     *         
***********************************************************************         
*                                                                               
XRCKWORK DSECT                                                                  
DYNAREA  DSECT                                                                  
         ds    Xl(SAVF4SA_LEN)                                                  
*                                                                               
WKSAVSUB DS  18fd                 INTERNAL  SUBROUTINE  REG  SAVE  AREA         
WKZOOMSV DS  16F                                                                
WKRESUME DS    F                                                                
*                                                                               
WKMR95WA DS    A                 "GVBMR95"  WORK  AREA  ADDRESS                 
*                                                                               
WKTIMWRK DS   0XL16                                                             
WKDBLWRK DS    D                  TEMPORARY DOUBLEWORD  WORK AREA               
WKDBLWK2 DS    D                  TEMPORARY DOUBLEWORD  WORK AREA               
WKDBLWK3 DS    D                  TEMPORARY DOUBLEWORD  WORK AREA               
*                                                                               
WKPLISTA DS    A                  CALLER    PROVIDED    PARAMETER  LIST         
*                                                                               
WKDDNAME DS    CL8                LOGICAL EVENT   FILE  DDNAME                  
WKGROUP  DS    CL8                                                              
WKOWNER  DS    CL8                                                              
WKVIEW#  DS    FL4                LOGICAL EVENT   FILE  VIEW#                   
WKGPLFID DS    FL4                LOGICAL EVENT   FILE  GPLFID                  
WKENVV   DS    CL32               ENVV                                          
WKDATETIME DS  CL16               DATETIME                                      
WKSTART  DS    CL32               START-UP DATA                                 
*                                                                               
WKFILCNT DS    F                  FILE    CONTROL BLOCK AREA                    
WKFILSIZ DS    F                                                                
WKFILBEG DS    A                                                                
WKFILMAX DS    A                                                                
WKFILELO DS    A                  OPEN FILE LIST ENTRY  FOR LOWEST KEY          
*                                                                               
*                                                                               
WKCALL64 DS    AD                 64-bit Event Record address                   
*                                                                               
*        ds   0d                                                                
WKCALLPL DS   0d                  LAST  EXIT CALL PARAMETER  LIST               
WKCALLP1 DS    A                  ENVIRONMENT  INFO ADDR                        
WKCALLP2 DS    A                  EVENT  FILE  INFO ADDR                        
WKCALLP3 DS    A                  STARTUP      DATA ADDR                        
WKCALLP4 DS    A                  EVENT RECORD  PTR ADDR                        
WKCALLP5 DS    A                  EXTRACT    RECORD ADDR                        
WKCALLP6 DS    A                  LOOK-UP    KEY    ADDR                        
WKCALLP7 DS    A                  WORK AREA POINTER ADDR                        
WKCALLP8 DS    A                  RETURN    CODE    ADDR                        
WKCALLP9 DS    A                  RETURNED  REC PTR ADDR                        
WKCALLPA DS    A                  RETURNED  REC LEN ADDR                        
WKCALLEN EQU   *-WKCALLPL         RETURNED  REC LEN ADDR                        
*                                                                               
REENTWK  DS    XL128              RE-ENTRANT PARAMETER   LIST                   
WKPRINT  DS    XL131           Print line                                       
WKTRACE  DS    CL1             Tracing                                          
WKDRUCK  DS    XL131           Print line                                       
         DS    CL1             Tracing                                          
WKPRINT2 DS    XL131           Print line (copy)                                
         DS    CL1             Tracing                                          
         DS   0F                                                                
WKREENT  DS    XL256           Reentrant workarea                               
WKDBLWK  DS    XL08            Double work workarea                             
*                                                                               
WKRECCNT DS    F               records running total                            
WKBUFCNT DS    F               records in block so far                          
WKPRFCNT0 DS   F               GROUP profiles running total                     
WKPRFCNT1 DS   F               DSN   profiles running total                     
WKBUFRET DS    F               blocks returned                                  
WKPARTS# DS    F                                                                
*                                                                               
WKRXITRC DS    F                  LAST  RXIT CALL RETURN CODE                   
WKCALLPT DS    ad                 RETURNED RECORD POINTER                       
WKCALLRL DS    F                  RETURNED RECORD LENGTH                        
*                                                                               
WKEOF    DS    CL1                                                              
WKGENRIC DS    XL1                                                              
WKEOFGR  DS    XL1                                                              
WKEOFDA  DS    XL1                                                              
WKTHRDNO DS    H                                                                
         DS    H                                                                
*                                                                               
WKRPTDCB DS    A                  CONTROL   REPORT      "DCB"                   
*                                                                               
         DS    0F                                                               
OUTDCB   DS    XL(OUTFILEL)    Reentrant DCB and DCBE areas                     
         DS    0F                                                               
OU2DCB   DS    XL(OU2FILEL)    Reentrant DCB and DCBE areas                     
         DS    0F                                                               
IN0DCB   DS    XL(IN0FILEL)    REENTRANT DCB AND DCBE AREAS                     
         DS    0F                                                               
IN1DCB   DS    XL(IN1FILEL)    REENTRANT DCB AND DCBE AREAS                     
*                                                                               
WKWTOPRM WTO   TEXT=(R2),MF=L                                                   
WKWTOLEN EQU   *-WKWTOPRM                                                       
*                                                                               
WK_MSG   GVBMSG PREFIX=WMSG,MF=L                                                
*                                                                               
         DS   0A                                                                
WKTXTBUF DS    CL135              Message buffer                                
WKTXTLEN DS    0HL002                                                           
WKPRTTXT DS    CL133                                                            
*                                                                               
         DS    0F                                                               
WKRETC   DS    F                                                                
*                                                                               
*        RECORD WORK AREAS                                                      
*                                                                               
         DS    0F                        ---------------                        
GRPAREA  DS    0CL131                                                           
GRPNAME  DS    CL8                       GROUP NAME                             
GRPUSER  DS    CL8                       MEMBER USERID                          
GRPOWNER DS    CL8                       OWNER  USERID                          
GRPACL   DS    XL1                                                              
         DS    XL106                                                            
         DS    0F                        ---------------                        
DS1AREA  DS    0CL131                                                           
DS1GRUP  DS    CL8                       GROUP OR USERID                        
DS1NAME  DS    CL44                      DSNAME                                 
DS1GENER DS    CL1                       GENERIC                                
DS1ACL   DS    CL1                       OWNER                                  
DS1OWNER DS    CL8                       OWNER                                  
         DS    XL69                                                             
         DS    0F                        ---------------                        
DS2AREA  DS    0CL131                                                           
DS2GRUP  DS    CL8                       GROUP OR USERID                        
DS2NAME  DS    CL44                      DSNAME                                 
DS2GENER DS    CL1                       GENERIC                                
DS2ACL   DS    CL1                       OWNER                                  
DS2OWNER DS    CL8                       OWNER                                  
DS2LUSR  DS    CL8                                                              
DS2VIAG  DS    CL8                       VIA GROUP                              
         DS    XL53                                                             
*                                                                               
         DS   0F                                                                
USERNUM  DS    F                         NUMBER OF USERS KEPT                   
USERTAB  DS  5957CL8                     USERS KEPT FROM LAST GROUP REF         
*                                                                               
*        RETURN BUFFER AREA                                                     
*                                                                               
WKREC    DS   0CL78                                                             
         DS 100CL78                                                             
*                                                                               
WORKLEN  EQU   (*-DYNAREA)                                                      
*                                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*        S T A R T U P   P A R A M E T E R   D A T A                  *         
*                                                                     *         
***********************************************************************         
STRTDATA DSECT                                                                  
*                                                                               
SDDDNPFX DS    CL04               DDNAME  PREFIX (_PARM, _INIT, _RPT)           
         DS    CL28                                                             
                        SPACE 3                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*        E X T R A C T   R E C O R D   A R E A                        *         
*                                                                     *         
***********************************************************************         
*                                                                               
EXTREC   DSECT                    EXTRACT RECORD  WORK   AREA                   
*                                                                               
EXRECLEN DS    HL02                                                             
         DS    XL02                                                             
EXSORTLN DS    HL02               SORT    KEY     LENGTH                        
EXTITLLN DS    HL02               SORT    TITLE   LENGTH                        
EXDATALN DS    HL02               EXTRACT DATA    LENGTH                        
EXNCOL   DS    HL02               NUMBER  OF  CALCULATED COLUMNS ("CT")         
EXVIEW#  DS    FL04               VIEW    NUMBER  (+X'80000000')                
EXPREFLN EQU   *-EXTREC                                                         
*                                                                               
         print off                                                              
         SYSSTATE ARCHLVL=2                                                     
         COPY  ASMMSP                                                           
LEAVE    OPSYN ASM_LEAVE                                                        
         asmmrel on                                                             
         print on                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*        REGISTER EQUATES:                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
         YREGS                                                                  
*                                                                               
         PRINT nogen                                                            
*                                                                               
GVBXR5   RMODE 24                                                               
GVBXR5   AMODE 31                                                               
GVBXR5   CSECT                                                                  
         J     CODE                                                             
XRCKEYE  GVBEYES GVBXR5                                                         
*                                                                               
static   loctr            set up the static loctr                               
code     loctr            followed by the code loctr                            
         using savf4sa,r13          map the save area                           
*        dc    h'0'                                                             
         stmg  R14,R12,SAVF4SAG64RS14 save callers registers GVBMR95            
*                                                                               
         llgtr R12,r15            SET   PROGRAM   BASE REGISTERS                
         USING (GVBXR5,code),R12                                                
*                                                                               
         llgtr r9,r1              LOAD  PARAMETER LIST ADDRESS                  
         USING GENPARM,R9                                                       
                                                                                
         llgt  R8,GPWORKA         LOAD  WORK  AREA ANCHOR ADDR                  
         ltgf  R1,0(,R8)          WORK  AREA  ALLOCATED ???                     
         JP    CHAIN              YES - CHAIN THEM TOGETHER (RSA'S)             
*                                                                               
         llgt  R0,FWORKLEN         LOAD   WORK AREA SIZE                        
         STORAGE OBTAIN,LENGTH=(0),LOC=24,CHECKZERO=YES GET WORKAREA            
         if    cij,r15,ne,x'14'   not zeroed?                                   
           lgr r10,r1             save address                                  
           lgr R0,R1              ZERO  WORK  AREA                              
           llgt R1,FWORKLEN                                                     
           sgr R14,R14                                                          
           sgr R15,R15                                                          
           MVCL R0,R14                                                          
           lgr r1,r10             restore pointer                               
         endif                                                                  
*                                                                               
         MVC   0(l'GVBXR5EY,R1),GVBXR5EY                                        
         aghi  r1,l'GVBXR5EY        move pointer past                           
         drop  r13                                                              
         USING DYNAREA,R1                                                       
         using savf4sa,dynarea                                                  
         stg   r13,savf4saprev    save current r13                              
         mvc   savf4said(4),=a(savf4said_value) set 'F4SA' in area              
         ST    R13,WKMR95WA       SAVE GVBMR95  WORK  AREA ADDRESS              
         lgr   R13,r1             Get new workarea into r13                     
         ST    R13,0(,R8)         SAVE  WORK  AREA ADDRESS                      
*                                                                               
         BRAS  R10,INITWORK       INITIALIZE WORK AREA                          
         LTR   R15,R15                                                          
         JNZ   RETURNX                                                          
         J     MAINLINE           BEGIN                                         
*                                                                               
CHAIN    ds    0h                                                               
         stg   r13,savf4saprev    save current r13                              
         llgtr R13,r1             Get new workarea into r13                     
         drop  r1                                                               
         using dynarea,r13                                                      
         using savf4sa,dynarea                                                  
*                                                                               
*                                                                               
***********************************************************************         
MAINLINE DS    0H                                                               
***********************************************************************         
         XC    WKRETC,WKRETC      Assume Rc=0                                   
*                                                                               
         LLGT  R8,GPENVA                                                        
         USING GENENV,R8                                                        
*                                                                               
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(20),=CL20'GVBXR5: PHASE IS: XX'                          
         MVC   WKPRINT+18(2),GPPHASE                                            
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         LLGT  R1,WKRESUME                                                      
         C     R1,=F'0'                                                         
         JL    JUMPLOW                                                          
         C     R1,=F'2'                                                         
         JH    JUMPHIGH                                                         
         SLL   R1,2                                                             
         XC    WKRESUME,WKRESUME                                                
         LAY   R15,JUMP                                                         
         AR    R15,R1                                                           
         BR    R15                                                              
JUMP     J     A0000              Goto RESUME(RESUMEPT)                         
         J     RESUMEP1                                                         
         J     RESUMEP2                                                         
*                                                                               
JUMPLOW  EQU   *                                                                
         DC    H'0'                                                             
JUMPHIGH EQU   *                                                                
         DC    H'0'                                                             
*                                                                               
A0000    EQU   *                                                                
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(37),=CL37'GVBXR5: NORMAL ENTRY, NO RESUME POINT'         
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         CLC   GPPHASE,=CL2'OP'                                                 
         JNE   A0200                                                            
         BRAS  R10,LOADB                                                        
         BRAS  R10,UNLOADB                                                      
         J     RETURNX                                                          
*                                                                               
A0200    EQU   *                                                                
         CLC   GPPHASE,=CL2'CL'                                                 
         JNE   A0300                                                            
         J     RETURNX                                                          
*                                                                               
A0300    EQU   *                                                                
         CLC   GPPHASE,=CL2'RD'                                                 
         JNE   A0400                                                            
         BRAS  R10,LOADB                                                        
         BRAS  R10,UNLOADB                                                      
         J     RETURNX                                                          
*                                                                               
A0400    EQU   *                                                                
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(29),=CL29'GVBXR5: UNRECOGNIZED PHASE XX'                 
         MVC   WKPRINT+27(2),GPPHASE                                            
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
         MVC   WKRETC,=F'16'                                                    
         J     RETURNX                                                          
*                                                                               
         DROP  R8                                                               
*                                                                               
***********************************************************************         
*   FILL   NEXT "BUFSIZE" buffer                                      *         
***********************************************************************         
LOADB    DS    0H                                                               
         CLI   WKEOF,C'Y'                                                       
         JNE   LOADB02                                                          
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(57),=CL57'GVBXR5: ALREADY AT END OF FILE. RETURN+        
               ING RC=8 TO GVBMR95'                                             
         LAY   R2,OUTDCB                                                        
         LAY   R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(35),=CL35'GVBXR5: BLOCKS RETURNED TO GVBMR95 '           
         L     R15,WKBUFRET                                                     
         CVD   R15,WKDBLWK                                                      
         MVC   WKPRINT+35(7),NUMMSK+5                                           
         MVI   WKPRINT+35,C' '                                                  
         ED    WKPRINT+35(7),WKDBLWK+5                                          
         LAY   R2,OUTDCB                                                        
         LAY   R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(35),=CL35'GVBXR5: GROUP PROFILES PROCESSED   '           
         L     R15,WKPRFCNT0                                                    
         CVD   R15,WKDBLWK                                                      
         MVC   WKPRINT+35(7),NUMMSK+5                                           
         MVI   WKPRINT+35,C' '                                                  
         ED    WKPRINT+35(7),WKDBLWK+5                                          
         LAY   R2,OUTDCB                                                        
         LAY   R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(35),=CL35'GVBXR5: DATASET PROFILES PROCESSED '           
         L     R15,WKPRFCNT1                                                    
         CVD   R15,WKDBLWK                                                      
         MVC   WKPRINT+35(7),NUMMSK+5                                           
         MVI   WKPRINT+35,C' '                                                  
         ED    WKPRINT+35(7),WKDBLWK+5                                          
         LAY   R2,OUTDCB                                                        
         LAY   R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         LAY   R2,OUTDCB                                                        
         MVC   WKREENT(8),OPENPARM                                              
         CLOSE ((R2)),MODE=31,MF=(E,WKREENT)                                    
*                                                                               
         MVC   WKRETC,=F'8'                                                     
         J     LOADB99                                                          
*                                                                               
LOADB02  EQU   *                                                                
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(26),=CL26'GVBXR5: LOADING I/O BUFFER'                    
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
***   PERFORM 2-WAY COLLATE OF GROUP MEMBERSHIP WITH DATASET LIST..             
*                                                                               
         LA    R7,100                                                           
         LAY   R5,WKREC           RETURN DATA HERE                              
         XC    USERNUM,USERNUM                                                  
*                                                                               
LOOP     EQU   *                         START OF LOOP                          
         CLC   GRPNAME,DS1GRUP                                                  
         JNH   A0002                                                            
         LA    R2,IN1DCB                 READ DS1   RECORD(S)                   
         LA    R0,DS1AREA                                                       
         GET   (R2),(R0)                                                        
         ASI   WKPRFCNT1,1                                                      
*                                                                               
         MVC   WKDRUCK,SPACES                                                   
         MVC   WKDRUCK(14),=CL14'GVBXR5: DSN  -'                                
         MVC   WKDRUCK+14(62),DS1AREA                                           
         LA    R2,OU2DCB                                                        
         LA    R0,WKDRUCK                                                       
         PUT   (R2),(R0)                                                        
         J     LOOP                                                             
*                                                                               
A0002    EQU   *                                                                
         CLC   GRPNAME,DS1GRUP                                                  
         JNL   A0004                                                            
         CLI   WKEOFGR,C'Y'                                                     
         JE    A0090                                                            
         LA    R2,IN0DCB                 READ GROUP RECORD(S)                   
         LA    R0,GRPAREA                                                       
         GET   (R2),(R0)                                                        
         ASI   WKPRFCNT0,1                                                      
*                                                                               
         MVC   WKDRUCK,SPACES                                                   
         MVC   WKDRUCK(14),=CL14'GVBXR5: GROUP-'                                
         MVC   WKDRUCK+14(25),GRPAREA                                           
         LA    R2,OU2DCB                                                        
         LA    R0,WKDRUCK                                                       
         PUT   (R2),(R0)                                                        
         J     LOOP                                                             
*                                                                               
A0004    EQU   *                       =>MUST BE EQUAL THEN                     
         CLC   WKGROUP,DS1GRUP           SAME GROUP, DIFFERENT DSN ?            
         JNE   A0008                     NO, GO                                 
*                                                                               
         MVC   WKDRUCK,SPACES                                                   
         MVC   WKDRUCK(37),=CL37'GVBXR5: retrieving members for group '         
         MVC   WKDRUCK+37(8),WKGROUP                                            
         LA    R2,OU2DCB                                                        
         LA    R0,WKDRUCK                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
*        COPY DATASET RECORD AND ADD STORED GROUP MEMBERS WITH ACCESS           
*                                                                               
         LA    R6,USERTAB                                                       
         L     R4,USERNUM                                                       
         LTR   R4,R4                                                            
         JP    A0006                                                            
         DC H'0'                         group with no members ?                
*                                                                               
A0006    EQU   *                                                                
         MVC   DS2GRUP,0(R6)             COPY USER TO OUTPUT                    
         MVC   DS2NAME,DS1NAME           COPY DSNAME                            
         MVC   DS2GENER,DS1GENER              GENERIC                           
         MVC   DS2ACL,DS1ACL                  ACL                               
         MVC   DS2OWNER,WKOWNER               OWNER                             
         MVC   DS2LUSR,0(R6)                  LUSR                              
         MVC   DS2VIAG,WKGROUP                GROUP                             
*                                                                               
         LA    R6,8(,R6)                                                        
         LA    R2,OUTDCB                 WRITE RECORD WITH USER ACCESS          
         LA    R0,DS2AREA                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         ASI   WKRECCNT,1                records running total                  
         ASI   WKBUFCNT,1                records in block so far                
         MVC   0(L'WKREC,R5),DS2GRUP                                            
         LA    R5,L'WKREC(,R5)                                                  
         BCTR  R7,0                                                             
         LTR   R7,R7                                                            
         JP    A0007                                                            
         MVC   WKRESUME,=F'1'                                                   
         MVC   WKPRINT2,WKPRINT                                                 
         STM   R14,R12,WKZOOMSV+4                                               
         J     A0088                                                            
RESUMEP1 EQU   *                                                                
         WTO 'RESUME POINT1'                                                    
         LM    R14,R12,WKZOOMSV+4                                               
         LA    R7,100                                                           
         LAY   R5,WKREC           RETURN DATA HERE                              
         MVC   WKPRINT,WKPRINT2                                                 
A0007    EQU   *                                                                
         BRCT  R4,A0006                                                         
*                                                                               
         LA    R2,IN1DCB                 READ DS1   RECORD(S)                   
         LA    R0,DS1AREA                                                       
         GET   (R2),(R0)                                                        
         ASI   WKPRFCNT1,1                                                      
*                                                                               
         MVC   WKDRUCK,SPACES                                                   
         MVC   WKDRUCK(14),=CL14'GVBXR5: DSN  *'                                
         MVC   WKDRUCK+14(62),DS1AREA                                           
         LA    R2,OU2DCB                                                        
         LA    R0,WKDRUCK                                                       
         PUT   (R2),(R0)                                                        
         J     A0004                                                            
*                                                                               
*     COPY DATASET RECORD AND ADD ALL GROUP MEMBERS WITH ACCESS....             
*                                                                               
A0008    EQU   *                                                                
         CLC   GRPNAME,DS1GRUP                                                  
         JNE   LOOP                                                             
*                                                                               
         MVC   WKDRUCK,SPACES                                                   
         MVC   WKDRUCK(43),=CL43'GVBXR5: retrieving and storing members+        
                for '                                                           
         MVC   WKDRUCK+43(8),GRPNAME                                            
         LA    R2,OU2DCB                                                        
         LA    R0,WKDRUCK                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         MVC   WKGROUP,GRPNAME           KEEP HOLD OF CURRENT GROUP             
         MVC   WKOWNER,GRPOWNER          KEEP HOLD OF CURRENT GROUP             
         XC    USERNUM,USERNUM                                                  
*                                                                               
         MVC   DS2NAME,DS1NAME           COPY DSNAME                            
         MVC   DS2GENER,DS1GENER              GENERIC                           
         MVC   DS2ACL,DS1ACL                  ACL                               
         MVC   DS2OWNER,WKOWNER               OWNER                             
         MVC   DS2VIAG,WKGROUP                GROUP                             
*                                                                               
         LA    R6,USERTAB                                                       
*                                                                               
A0010    EQU   *                                                                
         MVC   DS2GRUP,GRPUSER           COPY USER TO OUTPUT                    
         MVC   DS2LUSR,GRPUSER                LUSR                              
         MVC   0(8,R6),GRPUSER           KEEP LIST OF GROUP MEMBERS             
         LA    R6,8(,R6)                   IN CASE GROUP OCCURS AGAIN           
         ASI   USERNUM,1                 NUMBER OF GROUP MEMBERS                
*                                                                               
         LA    R2,OUTDCB                 WRITE RECORD WITH USER ACCESS          
         LA    R0,DS2AREA                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         ASI   WKRECCNT,1                records running total                  
         ASI   WKBUFCNT,1                records in block so far                
         MVC   0(L'WKREC,R5),DS2GRUP                                            
         LA    R5,L'WKREC(,R5)                                                  
         BCTR  R7,0                                                             
         LTR   R7,R7                                                            
         JP    A0011                                                            
         MVC   WKRESUME,=F'2'                                                   
         MVC   WKPRINT2,WKPRINT                                                 
         STM   R14,R12,WKZOOMSV+4                                               
         J     A0088                                                            
RESUMEP2 EQU   *                                                                
         WTO 'RESUME POINT2'                                                    
         LM    R14,R12,WKZOOMSV+4                                               
         LA    R7,100                                                           
         LAY   R5,WKREC           RETURN DATA HERE                              
         MVC   WKPRINT,WKPRINT2                                                 
A0011    EQU   *                                                                
*                                                                               
         CLI   WKEOFGR,C'Y'                                                     
         JE    A0090                                                            
         LA    R2,IN0DCB                 READ NEXT GROUP RECORD                 
         LA    R0,GRPAREA                                                       
         GET   (R2),(R0)                                                        
         ASI   WKPRFCNT0,1                                                      
*                                                                               
         MVC   WKDRUCK,SPACES                                                   
         MVC   WKDRUCK(14),=CL14'GVBXR5: GROUP*'                                
         MVC   WKDRUCK+14(25),GRPAREA                                           
         LA    R2,OU2DCB                                                        
         LA    R0,WKDRUCK                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         CLC   GRPNAME,WKGROUP                                                  
         JE    A0010                                                            
         JH    A0012                                                            
         DC    H'0'                      CAN'T BE LESS !!!                      
*                                                                               
A0012    EQU   *                                                                
         LA    R2,IN1DCB                 READ DS1   RECORD(S)                   
         LA    R0,DS1AREA                                                       
         GET   (R2),(R0)                                                        
         ASI   WKPRFCNT1,1                                                      
*                                                                               
         MVC   WKDRUCK,SPACES                                                   
         MVC   WKDRUCK(14),=CL14'GVBXR5: DSN  +'                                
         MVC   WKDRUCK+14(62),DS1AREA                                           
         LA    R2,OU2DCB                                                        
         LA    R0,WKDRUCK                                                       
         PUT   (R2),(R0)                                                        
         J     A0004                                                            
*                                                                               
*                                                                               
A0088    EQU   *                                                                
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(36),=CL36'GVBXR5: LOAD OF I/O BUFFER COMPLETED'          
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
         J     LOADB99                   block completed                        
*                                                                               
*        IT DOES MATTER IF THERE ARE NO MORE GROUPS AS THERE MAY BE             
*        MORE DATASETS TO READ, SO CONTINUE                                     
*                                                                               
IN0VEOF  EQU   *                                                                
         MVI   WKEOFGR,C'Y'                                                     
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(29),=CL29'GVBXR5: END OF GROUPS REACHED'                 
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         LA    R2,IN1DCB                 READ DS1   RECORD(S)                   
         LA    R0,DS1AREA                                                       
         GET   (R2),(R0)                                                        
         ASI   WKPRFCNT1,1                                                      
*                                                                               
         MVC   WKDRUCK,SPACES                                                   
         MVC   WKDRUCK(14),=CL14'GVBXR5: DSN  -'                                
         MVC   WKDRUCK+14(62),DS1AREA                                           
         LA    R2,OU2DCB                                                        
         LA    R0,WKDRUCK                                                       
         PUT   (R2),(R0)                                                        
         J     LOOP                                                             
*                                                                               
*        IT DOESN'T MATTER IF THERE ARE NO MORE DATASETS, WE'RE DONE            
*                                                                               
IN1VEOF  EQU   *                                                                
         MVI   WKEOFDA,C'Y'                                                     
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(31),=CL31'GVBXR5: END OF DATASETS REACHED'               
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
         J     A0090                                                            
*                                                                               
A0090    EQU   *                                                                
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(30),=CL30'GVBXR5: END OF DATA TO PROCESS'                
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(41),=CL41'GVBXR5: Records returned in this extra+        
               ct '                                                             
         L     R15,WKRECCNT                                                     
         CVD   R15,WKDBLWK                                                      
         MVC   WKPRINT+41(7),NUMMSK+5                                           
         MVI   WKPRINT+41,C' '                                                  
         ED    WKPRINT+41(7),WKDBLWK+5                                          
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         MVI   WKEOF,C'Y'                                                       
*                                                                               
         LAY   R2,OU2DCB                                                        
         MVC   WKREENT(8),OPENPARM                                              
         CLOSE ((R2)),MODE=31,MF=(E,WKREENT)                                    
*                                                                               
         LAY   R2,IN0DCB                                                        
         MVC   WKREENT(8),OPENPARM                                              
         CLOSE ((R2)),MODE=31,MF=(E,WKREENT)                                    
*                                                                               
         LAY   R2,IN1DCB                                                        
         MVC   WKREENT(8),OPENPARM                                              
         CLOSE ((R2)),MODE=31,MF=(E,WKREENT)                                    
*                                                                               
LOADB99  EQU   *                                                                
         BR    R10                                                              
*                                                                               
***********************************************************************         
*   RETURN NEXT "BUFSIZE" buffer                                      *         
***********************************************************************         
UNLOADB  DS    0H                                                               
         LLGT  R15,FWKREC                                                       
         MS    R15,WKBUFCNT                                                     
*                                                                               
         LLGT  R14,GPBLKSIZ                                                     
         USING GENBLKSZ,R14                                                     
         ST    R15,GP_RESULT_BLK_SIZE                                           
         DROP  R14                                                              
*                                                                               
         LAY   R1,WKREC                                                         
         LLGTR R1,R1                                                            
         LLGT  R14,GPBLOCKA       LOAD  POINTER ADDRESS                         
         USING GENBLOCK,R14                                                     
         STG   R1,GP_RESULT_PTR   RETURN BLOCK ADDRESS                          
         DROP  R14                                                              
*                                                                               
         IF (LTR,R15,R15,NZ)                                                    
           XC    WKBUFCNT,WKBUFCNT  RESET BUFFER COUNT = 0                      
           ASI   WKBUFRET,1         INCREMENT NUMBER BUFFERS RETURNED           
         ELSE                                                                   
           MVC   WKRETC,=F'8'       ALREADY EOF                                 
         ENDIF                                                                  
*                                                                               
         BR    R10                RETURN                                        
*                                                                               
***********************************************************************         
*   RETURN TO GVBMR95                                                 *         
***********************************************************************         
RETURNX  DS    0H                                                               
         LLGT  R15,WKRETC                                                       
         LLGT  R14,GPRTNCA        INDICATE NORMAL COMPLETION                    
         USING GENRTNC,R14                                                      
         ST    R15,GP_RETURN_CODE                                               
         DROP  R14                                                              
*                                                                               
         lg    r13,savf4saprev         restore caller save area                 
         lg    r14,SAVF4SAG64RS14      restore caller's r14                     
         lmg   r0,r12,SAVF4SAG64RS0    restore caller's r2- r12                 
         BSM   0,R14              RETURN                                        
*                                                                               
* NOT CURRENTLY USED -------------------------------------------------          
*                                                                               
RTNERROR ds    0h                                                               
         LLGT  R9,WKPLISTA                                                      
         USING GENPARM,R9                                                       
*                                                                               
*                                                                               
RTNERR_2 llgt  R3,GPENVA          LOAD   ENVIRONMENT INFO   ADDRESS             
         USING GENENV,R3                                                        
*                                                                               
         LH    R15,WKTXTBUF       RETURN TEXT LENGTH                            
         ST    R15,GP_ERROR_BUFFER_LEN                                          
*                                                                               
         BCTR  R15,0              DECREMENT   LENGTH FOR  "EX"                  
         LLGT  R14,GP_ERROR_BUFFER_PTR ERROR MESSAGE BUFFER ADDRESS             
         LA    R1,WKTXTBUF+4                                                    
         EXRL  R15,MVCR14R1                                                     
*                                                                               
         LGHI  R15,123            SET REASON CODE                               
         ST    R15,GP_ERROR_REASON                                              
         J     RETURNX                                                          
*                                                                               
         DROP  R3 GENENV                                                        
*                                                                               
***********************************************************************         
*        I N I T I A L I Z E   W O R K   A R E A                      *         
***********************************************************************         
*                                                                               
INITWORK DS    0H                                                               
         ST    R9,WKPLISTA        SAVE  PARAMETER  LIST   ADDRESS               
*                                                                               
         MVC   WKCALLPL(WKCALLEN),0(R9)  COPY READ EXIT PARAM LIST              
         LA    R0,WKRXITRC        RETURN     CODE                               
         ST    R0,WKCALLP8                                                      
         LA    R0,WKCALLPT        RETURNED RECORD  POINTER                      
         ST    R0,WKCALLP9                                                      
         LA    R0,WKCALLRL        RETURNED RECORD  LENGTH                       
         ST    R0,WKCALLPA                                                      
*                                                                               
         LAY   R14,MDLWTO         INITIALIZE  WTO  PARAMETER AREA               
         MVC   WKWTOPRM(WKWTOLEN),0(R14)                                        
*                                                                               
***********************************************************************         
*  SAVE DDNAME AND SET EVENT FILE ATTRIBUTES                          *         
***********************************************************************         
         LLGT  R14,GPFILEA        EVENT FILE DDNAME                             
         USING GENFILE,R14                                                      
         MVC   WKDDNAME,GPDDNAME                                                
         MVC   WKGPLFID,GPLFID                                                  
         MVC   GPRECLEN,FWKREC                                                  
         MVC   GPRECMAX,FWKREC                                                  
         L     R0,FWKREC                                                        
         MS    R0,=F'100'                                                       
         ST    R0,GPBLKMAX                                                      
         MVI   GPRECFMT,C'F'      FIXED      FORMAT   RECORDS                   
         XC    GPRECDEL,GPRECDEL                                                
         DROP  R14                                                              
*                                                                               
         LLGT  R14,GPENVA                                                       
         USING GENENV,R14                                                       
         MVC   WKVIEW#,GPVIEW#                                                  
         LLGT  R1,GPENVVA                                                       
         MVC   WKENVV,0(R1)                                                     
         MVC   WKDATETIME,GP_PROCESS_DATE_TIME                                  
         MVC   WKTHRDNO,GPTHRDNO                                                
         MVC   WKPARTS#,GP_PF_count                                             
         DROP  R14                                                              
*                                                                               
         LLGT  R14,GPSTARTA                                                     
         USING GENSTART,R14                                                     
         MVC   WKSTART,GP_STARTUP_DATA                                          
         DROP  R14                                                              
*                                                                               
         CLC   WKSTART(6),=CL6'TRACE='                                          
         JNE   I0010                                                            
         MVC   WKTRACE,WKSTART+6                                                
I0010    EQU   *                                                                
*                                                                               
*                                                                               
***********************************************************************         
*  OPEN MESSAGE FILE                                                  *         
***********************************************************************         
*      open message file                                                        
         LA    R14,outfile               COPY MODEL   DCB                       
d1       using ihadcb,outdcb                                                    
         MVC   outdcb(outfilel),0(R14)                                          
         lay   R0,outdcb                 SET  DCBE ADDRESS IN  DCB              
         aghi  R0,outfile0                                                      
         sty   R0,d1.DCBDCBE                                                    
*                                                                               
         LAY   R2,OUTDCB                                                        
         MVC   WKREENT(8),OPENPARM                                              
         OPEN  ((R2),(OUTPUT)),MODE=31,MF=(E,WKREENT)                           
*                                                                               
*      open message file                                                        
         LA    R14,ou2file               COPY MODEL   DCB                       
O2       using ihadcb,ou2dcb                                                    
         MVC   ou2dcb(ou2filel),0(R14)                                          
         lay   R0,ou2dcb                 SET  DCBE ADDRESS IN  DCB              
         aghi  R0,ou2file0                                                      
         sty   R0,O2.DCBDCBE                                                    
*                                                                               
         LAY   R2,OU2DCB                                                        
         MVC   WKREENT(8),OPENPARM                                              
         OPEN  ((R2),(OUTPUT)),MODE=31,MF=(E,WKREENT)                           
*                                                                               
*      OPEN INPUT FILE-0                                                        
         LA    R14,IN0FILE               COPY MODEL   DCB                       
I0       USING IHADCB,IN0DCB                                                    
         MVC   IN0DCB(IN0FILEL),0(R14)                                          
         LAY   R0,IN0DCB                 SET  DCBE ADDRESS IN  DCB              
         AGHI  R0,IN0FILE0                                                      
         STY   R0,I0.DCBDCBE                                                    
*                                                                               
         LAY   R2,IN0DCB                                                        
         MVC   WKREENT(8),OPENPARM                                              
         OPEN  ((R2),(INPUT)),MODE=31,MF=(E,WKREENT)                            
*                                                                               
*      OPEN INPUT FILE-1                                                        
         LA    R14,IN1FILE               COPY MODEL   DCB                       
I1       USING IHADCB,IN1DCB                                                    
         MVC   IN1DCB(IN1FILEL),0(R14)                                          
         LAY   R0,IN1DCB                 SET  DCBE ADDRESS IN  DCB              
         AGHI  R0,IN1FILE0                                                      
         STY   R0,I1.DCBDCBE                                                    
*                                                                               
         LAY   R2,IN1DCB                                                        
         MVC   WKREENT(8),OPENPARM                                              
         OPEN  ((R2),(INPUT)),MODE=31,MF=(E,WKREENT)                            
*                                                                @I1015         
*                                                                               
         LA    R2,IN0DCB                 READ GROUP RECORD(S)                   
         LA    R0,GRPAREA                                                       
         GET   (R2),(R0)                                                        
         ASI   WKPRFCNT0,1                                                      
*                                                                               
         MVC   WKDRUCK,SPACES                                                   
         MVC   WKDRUCK(14),=CL14'GVBXR5: GROUPI'                                
         MVC   WKDRUCK+14(25),GRPAREA                                           
         LA    R2,OU2DCB                                                        
         LA    R0,WKDRUCK                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         LA    R2,IN1DCB                 READ DS1   RECORD(S)                   
         LA    R0,DS1AREA                                                       
         GET   (R2),(R0)                                                        
         ASI   WKPRFCNT1,1                                                      
*                                                                               
         MVC   WKDRUCK,SPACES                                                   
         MVC   WKDRUCK(14),=CL14'GVBXR5: DSN  I'                                
         MVC   WKDRUCK+14(62),DS1AREA                                           
         LA    R2,OU2DCB                                                        
         LA    R0,WKDRUCK                                                       
         PUT   (R2),(R0)                                                        
*                                                                @I1015         
*      echo DDNAME                                                              
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(32),=CL32'GVBXR5: Being executed with DD:'               
         MVC   WKPRINT+32(8),WKDDNAME                                           
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                @I1015         
*      echo THRD#                                                               
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(33),=CL33'GVBXR5: Being executed in thread:'             
         LH    R15,WKTHRDNO                                                     
         CVD   R15,WKDBLWK                                                      
         MVC   WKPRINT+34(4),NUMMSK+8                                           
         MVI   WKPRINT+34,C' '                                                  
         ED    WKPRINT+34(4),WKDBLWK+6                                          
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                @I1015         
*      echo NUMBER PARTITIONS                                                   
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(32),=CL32'GVBXR5: Total number partitions:'              
         L     R15,WKPARTS#                                                     
         CVD   R15,WKDBLWK                                                      
         MVC   WKPRINT+33(4),NUMMSK+8                                           
         MVI   WKPRINT+33,C' '                                                  
         ED    WKPRINT+33(4),WKDBLWK+6                                          
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                @I1015         
*      echo VIEW#                                                               
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(33),=CL33'GVBXR5: Being executed with VIEW:'             
         L     R15,WKVIEW#                                                      
         CVD   R15,WKDBLWK                                                      
         MVC   WKPRINT+34(7),NUMMSK+5                                           
         MVI   WKPRINT+34,C' '                                                  
         ED    WKPRINT+34(7),WKDBLWK+5                                          
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                @I1015         
*      echo GPLFID                                                              
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(34),=CL34'GVBXR5: Being executed with LFID:'             
         L     R15,WKGPLFID                                                     
         CVD   R15,WKDBLWK                                                      
         MVC   WKPRINT+34(7),NUMMSK+5                                           
         MVI   WKPRINT+34,C' '                                                  
         ED    WKPRINT+34(7),WKDBLWK+5                                          
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                @I1015         
*      echo MR95ENVV                                                            
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(38),=CL38'GVBXR5: Being executed with MR95ENVV:'         
         MVC   WKPRINT+38(32),WKENVV                                            
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                @I1015         
*      echo DATETIME                                                            
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(33),=CL33'GVBXR5: Being executed DateTime:'              
         MVC   WKPRINT+33(16),WKDATETIME                                        
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                @I1015         
*      echo GP_STARTUP_DATA                                                     
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(37),=CL37'GVBXR5: Being executed with startup:'          
         MVC   WKPRINT+37(32),WKSTART                                           
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
*                                                                               
*                                                                               
I0099    EQU   *                                                                
         L     R15,WKRETC                                                       
         BR    R10                RETURN                                        
*                                                                               
         DROP  R9                                                               
*                                                                               
STATIC   LOCTR                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*        C O N S T A N T S                                            *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        CONSTANTS                                                              
*                                                                               
H1       DC    H'1'                                                             
H4       DC    H'4'                                                             
H8       DC    H'8'                                                             
F04      DC    F'04'                                                            
F40      DC    F'40'                                                            
FWKREC   DC    A(L'WKREC)                                                       
FWORKLEN DC    A(WORKLEN+8)                                                     
         ds    0d                                                               
MVCR14R1 MVC   0(0,R14),0(R1)     * * * * E X E C U T E D * * * *               
         ds    0d                                                               
CLCR1R14 CLC   0(0,R1),0(R14)     * * * * E X E C U T E D * * * *               
*                                                                               
NUMMSK   DC    XL12'402020202020202020202021'                                   
*                                                                               
         LTORG ,                                                                
*                                                                               
         DS   0D                                                                
MODE31   equ   X'8000'                                                          
         DS   0D                                                                
OPENPARM DC    XL8'8000000000000000'                                            
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*        D A T A   C O N T R O L   B L O C K S                        *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
OU2FILE  DCB   DSORG=PS,DDNAME=DDDRUCK,MACRF=(PM),DCBE=OU2FDCBE,       X        
               RECFM=FB,LRECL=131                                               
OU2FILE0 EQU   *-OU2FILE                                                        
OU2FDCBE DCBE  RMODE31=BUFF                                                     
OU2FILEL EQU   *-OU2FILE                                                        
*                                                                               
OUTFILE  DCB   DSORG=PS,DDNAME=DDPRINT,MACRF=(PM),DCBE=OUTFDCBE,       X        
               RECFM=FB,LRECL=131                                               
OUTFILE0 EQU   *-OUTFILE                                                        
OUTFDCBE DCBE  RMODE31=BUFF                                                     
OUTFILEL EQU   *-OUTFILE                                                        
*                                                                               
IN0FILE  DCB   DSORG=PS,DDNAME=DDGROUP,MACRF=(GM),DCBE=IN0FDCBE,       X        
               RECFM=FB,LRECL=131                                               
IN0FILE0 EQU   *-IN0FILE                                                        
IN0FDCBE DCBE  RMODE31=BUFF,EODAD=IN0VEOF                                       
IN0FILEL EQU   *-IN0FILE                                                        
*                                                                               
IN1FILE  DCB   DSORG=PS,DDNAME=DDDSNAME,MACRF=(GM),DCBE=IN1FDCBE,      X        
               RECFM=FB,LRECL=131                                               
IN1FILE0 EQU   *-IN1FILE                                                        
IN1FDCBE DCBE  RMODE31=BUFF,EODAD=IN1VEOF                                       
IN1FILEL EQU   *-IN1FILE                                                        
*                                                                               
MDLWTO   WTO   TEXT=(R2),MF=L     MODEL WTO TO DISPLAY CONSOLE MESSAGES         
*                                                                               
GVBXR5EY DC    CL8'GVBXR5'                                                      
SPACES   DC    CL256' '                                                         
XHEXFF   DC 1024X'FF'                                                           
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                 UNPACKED NUMERIC TRANSLATION MATRIX                           
***********************************************************************         
*                                                                               
*                    0 1 2 3 4 5 6 7 8 9 A B C D E F                            
*                                                                               
TRTACLVL DC    XL16'00D500000000000000C5000000000000'  00-0F                    
         DC    XL16'D9000000000000000000000000000000'  10-1F                    
         DC    XL16'E4000000000000000000000000000000'  20-2F                    
         DC    XL16'00000000000000000000000000000000'  30-3F                    
         DC    XL16'C3000000000000000000000000000000'  40-4F                    
         DC    XL16'00000000000000000000000000000000'  50-5F                    
         DC    XL16'00000000000000000000000000000000'  60-6F                    
         DC    XL16'00000000000000000000000000000000'  70-7F                    
         DC    XL16'C1000000000000000000000000000000'  80-8F                    
         DC    XL16'00000000000000000000000000000000'  90-9F                    
         DC    XL16'00000000000000000000000000000000'  A0-AF                    
         DC    XL16'00000000000000000000000000000000'  B0-BF                    
         DC    XL16'00000000000000000000000000000000'  C0-CF                    
         DC    XL16'00000000000000000000000000000000'  D0-DF                    
         DC    XL16'00000000000000000000000000000000'  E0-EF                    
         DC    XL16'00000000000000000000000000000000'  F0-FF                    
*                                                                               
TRTTBLU  DC    XL16'08080808080808080808080808080808'  00-0F                    
         DC    XL16'08080808080808080808080808080808'  10-1F                    
         DC    XL16'08080808080808080808080808080808'  20-2F                    
         DC    XL16'08080808080808080808080808080808'  30-3F                    
         DC    XL16'08080808080808080808080808080808'  40-4F                    
         DC    XL16'08080808080808080808080808080808'  50-5F                    
         DC    XL16'08080808080808080808080808080808'  60-6F                    
         DC    XL16'08080808080808080808080808080808'  70-7F                    
         DC    XL16'08080808080808080808080808080808'  80-8F                    
         DC    XL16'08080808080808080808080808080808'  90-9F                    
         DC    XL16'08080808080808080808080808080808'  A0-AF                    
         DC    XL16'08080808080808080808080808080808'  B0-BF                    
         DC    XL16'08080808080808080808080808080808'  C0-CF                    
         DC    XL16'08080808080808080808080808080808'  D0-DF                    
         DC    XL16'08080808080808080808080808080808'  E0-EF                    
         DC    XL16'00000000000000000000080808080808'  F0-FF                    
*                                                                               
         DS   0F                                                                
         DCBD  DSORG=PS                                                         
*                                                                               
         IHADCBE                                                                
*                                                                               
JFCBAR   DSECT                                                                  
         IEFJFCBN LIST=YES                                                      
*                                                                               
         CVT   DSECT=YES                                                        
*                                                                               
         IHAPSA                                                                 
         END                                                                    
