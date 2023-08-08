        TITLE 'GVBXR1 - READ EXIT - ICHEINTY RACF PROFILES'                     
**********************************************************************          
*                                                                               
* (C) COPYRIGHT IBM CORPORATION 2006, 2017.                                     
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
*                           ENVIRONMENT.                              *         
*                                                                     *         
*                         : THIS MODULE WILL READ DSN PROFILES VIA    *         
*                           ICHEINTY                                  *         
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
*        R13 - REGISTER  SAVE AREA    ADDRESS (GVBXR1  WORK AREA)     *         
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
*                                                                               
WKMR95WA DS    A                 "GVBMR95"  WORK  AREA  ADDRESS                 
WKZOOMSV DS  16F                                                                
WKRESUME DS    F                                                                
*                                                                               
WKTIMWRK DS   0XL16                                                             
WKDBLWRK DS    D                  TEMPORARY DOUBLEWORD  WORK AREA               
WKDBLWK2 DS    D                  TEMPORARY DOUBLEWORD  WORK AREA               
WKDBLWK3 DS    D                  TEMPORARY DOUBLEWORD  WORK AREA               
*                                                                               
WKPLISTA DS    A                  CALLER    PROVIDED    PARAMETER  LIST         
*                                                                               
WKDDNAME DS    CL8                LOGICAL EVENT   FILE  DDNAME                  
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
WKPRINT2 DS    XL131           Print line (saved copy)                          
         DS    CL1             Tracing                                          
         DS   0F                                                                
WKREENT  DS    XL256           Reentrant workarea                               
WKDBLWK  DS    XL08            Double work workarea                             
*                                                                               
WKRECCNT DS    F               records running total                            
WKBUFCNT DS    F               records in block so far                          
WKPRFCNT DS    F               profiles running total                           
WKBUFRET DS    F               blocks returned                                  
WKPARTS# DS    F                                                                
*                                                                               
WKRXITRC DS    F                  LAST  RXIT CALL RETURN CODE                   
WKCALLPT DS    ad                 RETURNED RECORD POINTER                       
WKCALLRL DS    F                  RETURNED RECORD LENGTH                        
*                                                                               
WKDSN    DS    CL44                                                             
WKEOF    DS    CL1                                                              
WKGENRIC DS    XL1                                                              
WKTHRDNO DS    H                                                                
*                                                                               
WKRPTDCB DS    A                  CONTROL   REPORT      "DCB"                   
*                                                                               
         DS    0F                                                               
OUTDCB   DS    XL(outfilel)    Reentrant DCB and DCBE areas                     
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
*        ICHEINTY STRUCTURES                                                    
*                                                                               
         DS    0F                                                               
DYNICH   DS    XL(ICHLEN)                DYNAMIC ICHEINTY AREA                  
         DS    0F                                                               
DYNACT   DS    XL(ACTLEN)                DYNAMIC ICHEACTN AREA                  
         DS    0F                                                               
DYNACT2  DS    XL(ACTLEN2)               DYNAMIC ICHEACTN AREA                  
         DS    0F                                                               
*                                                                               
*        ENTITYX STRUCTURE                                                      
*                                                                               
ENTBUFF  DS    0CL48                                                            
ENTBLEN  DS    H                                                                
ENTNLEN  DS    H                                                                
ENTNAME  DS    CL44                                                             
         DS    0F                                                               
WKRETC   DS    F                                                                
*                                                                               
*        RETURN WORK AREA                                                       
*                                                                               
         DS    0F                                                               
RETAREA  DS    0CL40                                                            
RETALEN  DS    F                         RETURN AREA LENGTH                     
RETDATA  DS    0CL36                                                            
RETRBA   DS    CL6                       RBA RETURN AREA                        
RETFLAGS DS    CL1                       FLAGS                                  
RETRES1  DS    CL1                       RESERVED                               
RETDDSC  DS    F                         DUPLICATE DATA SET NAME COUNT          
RETRES2  DS    CL8                       RESERVED                               
RETDLEN  DS    F                         RETURNED DATA LENGTH                   
RETOWNLN DS    F                         RETURNED OWNER NAME LENGTH             
RETOWNER DS    CL8                       RETURNED OWNER NAME                    
RETACLLN DS    F                         RETURNED DATA LENGTH                   
RETUSRLN DS    XL4              -------> RETURNED USR/GRP NAME LENGTH           
RETUSRID DS    CL8                       RETURNED USR/GRP                       
RETLVLLN DS    XL4                       RETURNED ACCESS LVL LENGTH             
RETLVLVL DS    XL1                       RETURNED ACCESS LVL                    
RETACTLN DS    XL4                       RETURNED COUNT LENGTH                  
RETACTVL DS    XL2                       RETURNED COUNT                         
ACLLEN   EQU   *-RETUSRLN       <-------                                        
         DS 5956XL(ACLLEN)                                                      
*                                                                               
*        RETURN BUFFER AREA                                                     
*                                                                               
         DS   0F                                                                
WKREC    DS   0CL62                                                             
         DS 400CL62                                                             
*                                                                               
WORKLEN  EQU   (*-DYNAREA)                                                      
*                                                                               
RECRETN  DSECT                                                                  
RECRDSN  DS    CL44                                                             
RECROWN  DS    CL8                                                              
RECRGEN  DS    CL1                                                              
RECRUSR  DS    CL8                                                              
RECRACL  DS    CL1                                                              
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
GVBXR1   RMODE 24                                                               
GVBXR1   AMODE 31                                                               
GVBXR1   CSECT                                                                  
         J     CODE                                                             
XRCKEYE  GVBEYES GVBXR1                                                         
*                                                                               
static   loctr            set up the static loctr                               
code     loctr            followed by the code loctr                            
         using savf4sa,r13          map the save area                           
*        dc    h'0'                                                             
         stmg  R14,R12,SAVF4SAG64RS14 save callers registers GVBMR95            
*                                                                               
         llgtr R12,r15            SET   PROGRAM   BASE REGISTERS                
         USING (GVBXR1,code),R12                                                
*                                                                               
         llgtr r9,r1              LOAD  PARAMETER LIST ADDRESS                  
         USING GENPARM,R9                                                       
                                                                                
         llgt  R8,GPWORKA         LOAD  WORK  AREA ANCHOR ADDR                  
         ltgf  R1,0(,R8)          WORK  AREA  ALLOCATED ???                     
         JP    CHAIN              YES - CHAIN THEM TOGETHER (RSA'S)             
*                                                                               
         llgt  r0,FWORKLEN        LOAD   WORK AREA SIZE                         
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
         MVC   0(l'GVBXR1EY,R1),GVBXR1EY                                        
         aghi  r1,l'GVBXR1EY        move pointer past                           
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
         MVC   WKPRINT(20),=CL20'GVBXR1: PHASE IS: XX'                          
         MVC   WKPRINT+18(2),GPPHASE                                            
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         LLGT  R1,WKRESUME                                                      
         C     R1,=F'0'                                                         
         JL    JUMPLOW                                                          
         C     R1,=F'1'                                                         
         JH    JUMPHIGH                                                         
         SLL   R1,2                                                             
         XC    WKRESUME,WKRESUME                                                
         LAY   R15,JUMP                                                         
         AR    R15,R1                                                           
         BR    R15                                                              
JUMP     J     A0000              Goto RESUME(RESUMEPT)                         
         J     RESUMEP1                                                         
*                                                                               
JUMPLOW  EQU   *                                                                
         DC    H'0'                                                             
JUMPHIGH EQU   *                                                                
         DC    H'0'                                                             
*                                                                               
A0000    EQU   *                                                                
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(37),=CL37'GVBXR1: NORMAL ENTRY, NO RESUME POINT'         
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
         MVC   WKPRINT(29),=CL29'GVBXR1: UNRECOGNIZED PHASE XX'                 
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
         MVC   WKPRINT(57),=CL57'GVBXR1: ALREADY AT END OF FILE. RETURN+        
               ING RC=8 TO GVBMR95'                                             
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(35),=CL35'GVBXR1: BLOCKS RETURNED TO GVBMR95 '           
         L     R15,WKBUFRET                                                     
         CVD   R15,WKDBLWK                                                      
         MVC   WKPRINT+35(7),NUMMSK+5                                           
         MVI   WKPRINT+35,C' '                                                  
         ED    WKPRINT+35(7),WKDBLWK+5                                          
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         MVC   WKRETC,=F'8'                                                     
         J     LOADB99                                                          
*                                                                               
LOADB02  EQU   *                                                                
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(26),=CL26'GVBXR1: LOADING I/O BUFFER'                    
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
*        LOOP TO RETRIEVE ALL DATA SET PROFILES                                 
*          FOR EACH HIGH LEVEL QUALIFIER, GENERIC PROFILES ARE                  
*            RETRIEVED FIRST                                                    
*                                                                               
         LA    R4,400                                                           
         LAY   R5,WKREC           RETURN DATA HERE                              
         USING RECRETN,R5                                                       
A0008    EQU   *                         START OF LOOP                          
         XC    RETDATA,RETDATA           CLEAR ICHEINTY RETURN DATA             
         ICHEINTY NEXTC,ENTRYX=ENTBUFF,RELEASE=1.9,MF=(E,DYNICH)                
         LTR   15,15                     CHECK RETURN CODE                      
         JNZ   A0090                     EXIT ON NON ZERO RETURN CODE           
*               .                                                               
*        PROCESS DATA SET PROFILES                                              
*               .                                                               
         TM    RETFLAGS,X'80'            CHECK GENERIC BIT                      
         JO    GENERIC                   BRANCH IF GENERIC BIT IS ON            
         ICHEINTY OPTIONS=(NOEXEC),GENERIC=NO,MF=(E,DYNICH)                     
         MVI   WKGENRIC,C' '                                                    
         J     A0010                     PROCESS NEXT PROFILE                   
*                                                                               
GENERIC  EQU   *                         PROFILE NAME IS GENERIC                
         ICHEINTY OPTIONS=(NOEXEC),GENERIC=UNCOND,MF=(E,DYNICH)                 
         MVI   WKGENRIC,C'G'                                                    
*                                                                               
A0010    EQU   *                                                                
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(8),=CL8'GVBXR1: '                                        
         LA    R1,ENTNAME                                                       
         LA    R14,WKDSN                                                        
         MVC   WKDSN,SPACES                                                     
         LH    R15,ENTNLEN                                                      
         BCTR  R15,0                                                            
         EX    R15,MVCR14R1                                                     
         MVC   WKPRINT+8(8),WKDSN                                               
         MVC   WKPRINT+54(08),RETOWNER                                          
         MVC   WKPRINT+64(01),WKGENRIC                                          
*                                                                               
         LR    R6,R13                  STARTING POINT                           
         A     R6,RETACLLN             LENGTH OF GROUP DATA                     
         LR    R3,R13                                                           
A0012    EQU   *                                                                
         MVC   WKPRINT+66(8),RETUSRID-DYNAREA(R3)                               
         MVC   WKPRINT+76(1),RETLVLVL-DYNAREA(R3)                               
         TR    WKPRINT+76(1),TRTACLVL                                           
*                                                                               
         IF (CLI,WKTRACE,GE,C'2')                                               
           LA    R2,OUTDCB                                                      
           LA    R0,WKPRINT                                                     
           PUT   (R2),(R0)                                                      
         ENDIF                                                                  
*                                                                               
         ASI   WKRECCNT,1                records running total                  
         ASI   WKBUFCNT,1                records in block so far                
         MVC   RECRDSN,WKDSN                                                    
         MVC   RECROWN,RETOWNER                                                 
         MVC   RECRGEN,WKGENRIC                                                 
         MVC   RECRUSR,RETUSRID-DYNAREA(R3)                                     
         MVC   RECRACL,WKPRINT+76                                               
*                                                                               
         LA    R5,L'WKREC(,R5)                                                  
         BCTR  R4,0                      subtract 1 off remaing recs            
         LTR   R4,R4                                                            
         JP    A0014                                                            
         MVC   WKRESUME,=F'1'                                                   
         MVC   WKPRINT2,WKPRINT                                                 
         STM   R14,R12,WKZOOMSV+4                                               
         J     A0088                                                            
RESUMEP1 EQU   *                                                                
         WTO 'RESUME POINT1'                                                    
         LM    R14,R12,WKZOOMSV+4                                               
         LA    R4,400                                                           
         LAY   R5,WKREC           RETURN DATA HERE                              
         MVC   WKPRINT,WKPRINT2                                                 
*                                                                               
A0014    EQU   *                                                                
*        MVC   WKPRINT,SPACES                                                   
*        MVC   WKPRINT(22),=CL22'GVBXR1: ACL RETRIEVED '                        
*        MVC   WKPRINT+22(8),RETUSRID-DYNAREA(R3)                               
*        LA    R2,OUTDCB                                                        
*        LA    R0,WKPRINT                                                       
*        PUT   (R2),(R0)                                                        
*                                                                               
         LA    R3,ACLLEN(,R3)                                                   
         CR    R3,R6                                                            
         JL    A0012                     process next user acl                  
*                                                                               
*        MVC   WKPRINT,SPACES                                                   
*        MVC   WKPRINT(27),=CL27'GVBXR1: DATASET PROCCESSED '                   
*        MVC   WKPRINT+27(44),WKDSN                                             
*        LA    R2,OUTDCB                                                        
*        LA    R0,WKPRINT                                                       
*        PUT   (R2),(R0)                                                        
*                                                                               
         ASI   WKPRFCNT,1                                                       
         J     A0008                                                            
*                                                                               
A0088    EQU   *                                                                
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(32),=CL32'GVBXR1: LOAD I/O BLOCK COMPLETED'              
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
         J     LOADB99                   block completed                        
*                                                                               
A0090    EQU   *                                                                
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(20),=CL20'GVBXR1: END OF FILE.'                          
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(41),=CL41'GVBXR1: Records returned in this extra+        
               ct '                                                             
         L     R15,WKRECCNT                                                     
         CVD   R15,WKDBLWK                                                      
         MVC   WKPRINT+43(7),NUMMSK+5                                           
         MVI   WKPRINT+43,C' '                                                  
         ED    WKPRINT+43(7),WKDBLWK+5                                          
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(43),=CL43'GVBXR1: Profiles processed in this ext+        
               ract '                                                           
         L     R15,WKPRFCNT                                                     
         CVD   R15,WKDBLWK                                                      
         MVC   WKPRINT+43(7),NUMMSK+5                                           
         MVI   WKPRINT+43,C' '                                                  
         ED    WKPRINT+43(7),WKDBLWK+5                                          
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
         MVI   WKEOF,C'Y'                                                       
*                                                                               
LOADB99  EQU   *                                                                
         L     R15,WKRETC                                                       
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
*                                                                @I1015         
*      echo DDNAME                                                              
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(32),=CL32'GVBXR1: Being executed with DD:'               
         MVC   WKPRINT+32(8),WKDDNAME                                           
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                @I1015         
*      echo THRD#                                                               
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(33),=CL33'GVBXR1: Being executed in thread:'             
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
         MVC   WKPRINT(32),=CL32'GVBXR1: Total number partitions:'              
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
         MVC   WKPRINT(33),=CL33'GVBXR1: Being executed with VIEW:'             
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
         MVC   WKPRINT(34),=CL34'GVBXR1: Being executed with LFID:'             
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
         MVC   WKPRINT(38),=CL38'GVBXR1: Being executed with MR95ENVV:'         
         MVC   WKPRINT+38(32),WKENVV                                            
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                @I1015         
*      echo DATETIME                                                            
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(33),=CL33'GVBXR1: Being executed DateTime:'              
         MVC   WKPRINT+33(16),WKDATETIME                                        
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                @I1015         
*      echo GP_STARTUP_DATA                                                     
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(37),=CL37'GVBXR1: Being executed with startup:'          
         MVC   WKPRINT+37(32),WKSTART                                           
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
***********************************************************************         
*  SET UP ICHEINTY                                                    *         
***********************************************************************         
         MVC   ENTNLEN,H1                SET ENTITY LENGTH TO 1                 
         MVC   ENTBLEN,H44               SET BUFFER LENGTH TO 44                
         XC    ENTNAME,ENTNAME           CLEAR ENTITY NAME AREA                 
         MVC   RETALEN,F5120             SET RETURN AREA LENGTH                 
*                                                                               
*        COPY STATIC ICHEINTY AND ICHEACTN TO DYNAMIC GETMAINED AREAS           
*                                                                               
         MVC   DYNICH(ICHLEN),STATICH                                           
         MVC   DYNACT(ACTLEN),STATACT                                           
         MVC   DYNACT2(ACTLEN2),STATACT2                                        
         ICHEINTY RELEASE=1.9,ACTIONS=(DYNACT,DYNACT2),                *        
               WKAREA=RETAREA,                                         *        
               OPTIONS=(FLDEF,NOEXEC),GENERIC=NO,MF=(E,DYNICH)                  
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
*        STATIC ICHEACTN AND ICHEINTY AREAS                                     
*                                                                               
STATACT  ICHEACTN FIELD=OWNER                                                   
ACTLEN   EQU   *-STATACT                 LENGTH OF ICHEACTN                     
STATACT2 ICHEACTN FIELD=ACL                                                     
ACTLEN2  EQU   *-STATACT2                LENGTH OF ICHEACTN                     
*                                                                               
STATICH  ICHEINTY NEXTC,TYPE='DS',ENTRYX=*-*,RELEASE=1.9,DATAMAP=NEW,  *        
               ACTIONS=(STATACT,STATACT2),WKAREA=*-*,MF=L                       
ICHLEN   EQU   *-STATICH                 LENGTH OF ICHEINTY                     
*                                                                               
*        CONSTANTS                                                              
*                                                                               
H1       DC    H'1'                                                             
H4       DC    H'4'                                                             
H44      DC    H'44'                                                            
F04      DC    F'04'                                                            
F40      DC    F'40'                                                            
F5120    DC    F'5120'                                                          
FWKREC   DC    A(L'WKREC)                                                       
FWORKLEN DC    A(WORKLEN+8)                                                     
         ds    0d                                                               
MVCR14R1 MVC   0(0,R14),0(R1)     * * * * E X E C U T E D * * * *               
         ds    0d                                                               
CLCR1R14 CLC   0(0,R1),0(R14)     * * * * E X E C U T E D * * * *               
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
outfile  DCB   DSORG=PS,DDNAME=DDPRINT,MACRF=(PM),DCBE=outfdcbe,       x        
               RECFM=FB,LRECL=131                                               
outfile0 EQU   *-outfile                                                        
outfdcbe DCBE  RMODE31=BUFF                                                     
outfilel EQU   *-outfile                                                        
*                                                                               
MDLWTO   WTO   TEXT=(R2),MF=L     MODEL WTO TO DISPLAY CONSOLE MESSAGES         
*                                                                               
GVBXR1EY DC    CL8'GVBXR1'                                                      
SPACES   DC    CL256' '                                                         
XHEXFF   DC 1024X'FF'                                                           
*                                                                               
         LTORG ,                                                                
*                                                                               
NUMMSK   DC    XL12'402020202020202020202021'                                   
*                                                                               
*******************************************************                         
*                 UNPACKED NUMERIC TRANSLATION MATRIX                           
*******************************************************                         
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
