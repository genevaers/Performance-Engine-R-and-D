        TITLE 'GVBXR7 - READ EXIT - QSAM I/O TP90'                              
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
*                         : THIS MODULE WILL READ RECORDS VIA QSAM    *         
*                                                                     *         
*  GENEVA MODULES USED    : GVBTP90 - i/o module                      *         
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
*        R13 - REGISTER  SAVE AREA    ADDRESS (GVBXR7  WORK AREA)     *         
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
         ds    Xl(SAVF4SA_LEN)                                                  
*                                                                               
WKSAVSUB DS  18fd                 INTERNAL  SUBROUTINE  REG  SAVE  AREA         
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
         DS   0F                                                                
WKREENT  DS    XL256           Reentrant workarea                               
WKDBLWK  DS    XL08            Double work workarea                             
*                                                                               
WKOUTREC DS    A                  CURRENT   OUTPUT     RECORD                   
*                                                                               
WKTOKNRC DS    A                  NAME/TOKEN  SERVICES RETURN CODE              
WKTOKNAM DS    XL16               TOKEN NAME                                    
WKTOKN   DS   0XL16               TOKEN VALUE                                   
WKTOKNGB DS    A                  GLOBAL AREA                                   
         DS    A                                                                
         DS    A                                                                
         DS    A                                                                
*                                                                               
GVBTP90  DS    A                 "GVBTP90" ADDRESS                              
TP90LIST DS   0A               PARAMETER  LIST FOR "STGTP90"                    
TP90PA   DS    A               ADDRESS OF PARAMETER AREA                        
TP90RECA DS    A               ADDRESS OF RECORD    BUFFER                      
TP90KEYA DS    A               ADDRESS OF RECORD    KEY                         
*                                                                               
TP90AREA DS   0CL01            FILE SPECIFICATION ENTRY DEFINITION              
*                              FOR GVBTP90                                      
PAANCHOR DS    AL04            TP90   WORK  AREA ANCHOR                         
PADDNAME DS    CL08            FILE   DDNAME                                    
PAFUNC   DS    CL02            FUNCTION CODE                                    
PAFTYPE  DS    CL01            FILE   TYPE (V  = VSAM,  S = SEQUENTIAL)         
PAFMODE  DS    CL02            FILE   MODE (I  = INPUT, O = OUTPUT    )         
*                                          (IO = BOTH                 )         
PARTNC   DS    CL01            RETURN CODE                                      
PAVSAMRC DS    HL02            VSAM   RETURN CODE                               
PARECLEN DS    HL02            RECORD LENGTH                                    
PARECFMT DS    CL01            RECORD FORMAT (F=FIXED, V=VARIABLE)              
PARESDS  DS    CL01            ESDA : direct access                             
*                                                                               
WKEOF    DS    CL1                                                              
         DS    XL1                                                              
WKTHRDNO DS    H                                                                
*                                                                               
         DS    0F                                                               
WKGLOBA  DS    F                  ADDR GLOBAL AREA                              
WKPARTS# DS    F                                                                
WKRECCNT DS    F                  NUMBER OF RECORDS READ                        
WKBUFCNT DS    F                  NUMBER OF RECORDS IN BUFFER                   
WKBUFRET DS    F                  NUMBER OF BUFFERS RETURNED                    
WKRXITRC DS    F                  LAST  RXIT CALL RETURN CODE                   
WKCALLPT DS    ad                 RETURNED RECORD POINTER                       
WKCALLRL DS    F                  RETURNED RECORD LENGTH                        
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
         DS    0F                                                               
WKRETC   DS    F                                                                
WKKEY    DS    CL96                                                             
WKREC    DS    CL96                                                             
         DS  99CL96                                                             
*                                                                               
WORKLEN  EQU   (*-XRCKWORK)                                                     
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*        E X T R A C T   R E C O R D   A R E A                        *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*     C O M M O N   L A N G U A G E   E N V I R O N M E N T   A R E A *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LEINTER  DSECT                                                                  
*                                                                               
LEPARM   DS   0A                                                                
LEFUNCA  DS    A                  FUNCTION CODE                                 
LESUBRA  DS    A                  ENTRY    POINT   ADDRESS                      
LETOKNA  DS    A                  LANGUAGE ENVIRONMENT TOKEN ADDRESS            
LEPARMA  DS    A                  CALLED   MODULE  PARAMETER LIST               
LERTNCA  DS    A                  RETURN   CODE                                 
LEREASA  DS    A                  REASON   CODE                                 
LEFDBKA  DS    A                  FEEDBACK AREA                                 
*                                                                               
LECEEADR DS    A                  LANGUAGE ENVIRONMENT INTERFACE  ADDR          
LETOKEN  DS    A                  LANGUAGE ENVIRONMENT TOKEN                    
LESUBADR DS   2A                  CALLED   ENTRY POINT ADDR   (DBLWORD)         
LEPARMP  DS    A                  PARAMETER LIST ADDR                           
LERTNC   DS    F                  RETURN   CODE                                 
LEREASON DS    F                  REASON   CODE                                 
LERETADR DS    A                  RETURN   ADDRESS                              
LEFDBK   DS   3A                  FEEDBACK AREA                                 
*                                                                               
LERDECBA DS    A                  "GENPIPE/GENREAD"    DECB  LIST               
LEWDECBA DS    A                  "GENPIPE" PIPE WRITE DECB  LIST               
LELRECL  DS    F                  READ EXIT RETURNS                             
LERECFM  DS    X                  READ EXIT RETURNS                             
         DS    X                  (FILLER)                                      
*                                                                               
LEREGSAV DS  18F                  REGISTER  SAVE AREA                           
*                                                                               
LEINTLEN EQU   *-LEINTER                                                        
*                                                                               
GLOBAREA DSECT                                                                  
GLOREC#  DS    F                  TOTAL RECORDS                                 
GLOPARTP DS    F                  PARTITIONS PROCESSED                          
GLOPARTT DS    F                  PARTITIONS TOTAL                              
GLOBLEN  EQU   *-GLOBAREA                                                       
*                                                                               
         print off                                                              
         SYSSTATE ARCHLVL=2                                                     
         COPY  ASMMSP                                                           
LEAVE    OPSYN ASM_LEAVE                                                        
         asmmrel on                                                             
         print on                                                               
*                                                                               
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
GVBXR7   RMODE 24                                                               
GVBXR7   AMODE 31                                                               
GVBXR7   CSECT                                                                  
         J     CODE                                                             
XRCKEYE  GVBEYES GVBXR7                                                         
*                                                                               
static   loctr            set up the static loctr                               
code     loctr            followed by the code loctr                            
         using savf4sa,r13          map the save area                           
*        dc    h'0'                                                             
         stmg  R14,R12,SAVF4SAG64RS14 save callers registers GVBMR95            
*                                                                               
         llgtr R12,r15            SET   PROGRAM   BASE REGISTERS                
         USING (GVBXR7,code),R12                                                
*                                                                               
         llgtr r9,r1              LOAD  PARAMETER LIST ADDRESS                  
         USING GENPARM,R9                                                       
                                                                                
         llgt  R8,GPWORKA         LOAD  WORK  AREA ANCHOR ADDR                  
         ltgf  R1,0(,R8)          WORK  AREA  ALLOCATED ???                     
         JP    CHAIN              YES - CHAIN THEM TOGETHER (RSA'S)             
*                                                                               
         lghi  R0,WORKLEN+8       LOAD   WORK AREA SIZE                         
         STORAGE OBTAIN,LENGTH=(0),LOC=24,CHECKZERO=YES GET WORKAREA            
         if    cij,r15,ne,x'14'   not zeroed?                                   
           lgr r10,r1             save address                                  
           lgr R0,R1              ZERO  WORK  AREA                              
           lghi R1,WORKLEN+8                                                    
           sgr R14,R14                                                          
           sgr R15,R15                                                          
           MVCL R0,R14                                                          
           lgr r1,r10             restore pointer                               
         endif                                                                  
*                                                                               
         MVC   0(l'GVBXR7EY,R1),GVBXR7EY                                        
         aghi  r1,l'GVBXR7EY        move pointer past                           
         drop  r13                                                              
         USING XRCKWORK,R1                                                      
         using savf4sa,xrckwork                                                 
         stg   r13,savf4saprev    save current r13                              
         mvc   savf4said(4),=a(savf4said_value) set 'F4SA' in area              
         ST    R13,WKMR95WA       SAVE GVBMR95  WORK  AREA ADDRESS              
         lgr   R13,r1             Get new workarea into r13                     
         ST    R13,0(,R8)         SAVE  WORK  AREA ADDRESS                      
*                                                                               
         BRAS  R10,INITWORK       INITIALIZE WORK AREA                          
         LTR   R15,R15            Returns r4 => work area                       
         JNZ   RETURNX                                                          
         J     MAINLINE           BEGIN                                         
*                                                                               
CHAIN    ds    0h                                                               
         stg   r13,savf4saprev    save current r13                              
         llgtr R13,r1             Get new workarea into r13                     
         drop  r1                                                               
         using xrckwork,r13                                                     
         using savf4sa,xrckwork                                                 
         llgt  r4,wkgloba                                                       
         using globarea,r4                                                      
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
         CLC   GPPHASE,=CL2'OP'                                                 
         JNE   A0200                                                            
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(18),=CL18'GVBXR7: OPEN PHASE'                            
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
         BRAS  R10,LOADB                                                        
         BRAS  R10,UNLOADB                                                      
         J     RETURNX                                                          
*                                                                               
A0200    EQU   *                                                                
         CLC   GPPHASE,=CL2'CL'                                                 
         JNE   A0300                                                            
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(19),=CL19'GVBXR7: CLOSE PHASE'                           
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
         J     RETURNX                                                          
*                                                                               
A0300    EQU   *                                                                
         CLC   GPPHASE,=CL2'RD'                                                 
         JNE   A0400                                                            
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(18),=CL18'GVBXR7: READ PHASE'                            
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
         BRAS  R10,LOADB                                                        
         BRAS  R10,UNLOADB                                                      
         J     RETURNX                                                          
*                                                                               
A0400    EQU   *                                                                
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(29),=CL29'GVBXR7: UNRECOGNIZED PHASE XX'                 
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
         JNE   LOADB10                                                          
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(57),=CL57'GVBXR7: ALREADY AT END OF FILE. RETURN+        
               ING RC=8 TO GVBMR95'                                             
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(35),=CL35'GVBXR7: BLOCKS RETURNED TO GVBMR95 '           
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
LOADB10  EQU   *                                                                
         MVC   PAFUNC,=CL2'RD'    INITIALIZE FUNCTION CODE                      
         MVC   PADDNAME,WKDDNAME     '       DDNAME                             
         MVI   PAFTYPE,C'S'       FILE TYPE                                     
         MVC   PAFMODE,=CL2'I '   FILE MODE                                     
         MVI   PARECFMT,C'F'      FORMAT                                        
*                                                                               
         LA    R11,100                                                          
LOADB20  EQU   *               ---READ BLOCK OF RECORDS                         
         LA    R0,WKREC           RETURN DATA HERE                              
         L     R1,=F'96'                                                        
         MS    R1,WKBUFCNT                                                      
         AR    R0,R1                                                            
         ST    R0,TP90RECA                                                      
         LA    R1,TP90LIST        POINT R1 TO PARAMETER LIST                    
*        L     R15,=V(GVBTP90)    LOAD  ADDRESS OF "GVBTP90"                    
         L     R15,GVBTP90        LOAD  ADDRESS OF "GVBTP90"                    
         BASR  R14,R15            CALL  I/O ROUTINE                             
*                                                                               
         IF (LTR,R15,R15,NZ)                                                    
           MVC   WKPRINT,SPACES                                                 
           IF (CLC,PARTNC,EQ,=C'2')                                             
             MVC   WKPRINT,SPACES                                               
             MVC   WKPRINT(20),=CL20'GVBXR7: END OF FILE.'                      
             LA    R2,OUTDCB                                                    
             LA    R0,WKPRINT                                                   
             PUT   (R2),(R0)                                                    
             MVC   WKPRINT,SPACES                                               
             MVC   WKPRINT(39),=CL39'GVBXR7: Records read in this parti+        
               tion:'                                                           
             L     R15,WKRECCNT                                                 
             CVD   R15,WKDBLWK                                                  
             MVC   WKPRINT+40(7),NUMMSK+5                                       
             MVI   WKPRINT+40,C' '                                              
             ED    WKPRINT+40(7),WKDBLWK+5                                      
             LA    R2,OUTDCB                                                    
             LA    R0,WKPRINT                                                   
             PUT   (R2),(R0)                                                    
*                                                                               
             L     R0,GLOREC#                Maintain global rec count          
             A     R0,WKRECCNT                                                  
             ST    R0,GLOREC#                                                   
             L     R0,GLOPARTP               Maintain global part count         
             A     R0,=F'1'                                                     
             ST    R0,GLOPARTP                                                  
*                                                                               
             IF (CLC,GLOPARTP,GE,GLOPARTT)                                      
               MVC   WKPRINT,SPACES                                             
               MVC   WKPRINT(39),=CL39'GVBXR7: Records read in all part+        
               itions:'                                                         
               L     R15,GLOREC#                                                
               CVD   R15,WKDBLWK                                                
               MVC   WKPRINT+40(7),NUMMSK+5                                     
               MVI   WKPRINT+40,C' '                                            
               ED    WKPRINT+40(7),WKDBLWK+5                                    
               LA    R2,OUTDCB                                                  
               LA    R0,WKPRINT                                                 
               PUT   (R2),(R0)                                                  
               MVC   WKPRINT,SPACES                                             
               MVC   WKPRINT(39),=CL39'GVBXR7: Number of partitions pro+        
               cessed:'                                                         
               L     R15,GLOPARTP                                               
               CVD   R15,WKDBLWK                                                
               MVC   WKPRINT+40(7),NUMMSK+5                                     
               MVI   WKPRINT+40,C' '                                            
               ED    WKPRINT+40(7),WKDBLWK+5                                    
               LA    R2,OUTDCB                                                  
               LA    R0,WKPRINT                                                 
               PUT   (R2),(R0)                                                  
             ENDIF                                                              
*                                                                               
             MVI   WKEOF,C'Y'                                                   
*                                                                               
           ELSE                                                                 
             MVC   WKPRINT(30),=CL30'GVBXR7: Read Error xxxx/xxxx X'            
             MVC   WKPRINT+29(1),PARTNC                                         
             XGR   R15,R15                                                      
             ICM   R15,B'0001',PAVSAMRC+1                                       
             CVD   R15,WKDBLWK                                                  
             MVC   WKPRINT+19(4),NUMMSK+8                                       
             MVI   WKPRINT+19,C' '                                              
             ED    WKPRINT+19(4),WKDBLWK+6                                      
             ICM   R15,B'0001',PAVSAMRC                                         
             CVD   R15,WKDBLWK                                                  
             MVC   WKPRINT+24(4),NUMMSK+8                                       
             MVI   WKPRINT+24,C' '                                              
             ED    WKPRINT+24(4),WKDBLWK+6                                      
             LA    R2,OUTDCB                                                    
             LA    R0,WKPRINT                                                   
             PUT   (R2),(R0)                                                    
*                                                                               
             MVC   WKRETC,=F'16'                                                
           ENDIF                                                                
           J     LOADB99                                                        
         ELSE                                                                   
           ASI   WKBUFCNT,1                                                     
           ASI   WKRECCNT,1                                                     
*                                                                               
           IF (CLI,WKTRACE,GE,C'2')                                             
             MVC   WKPRINT,SPACES                                               
             MVC   WKPRINT(20),=CL20'GVBXR7: RECORD READ '                      
             L     R1,TP90RECA                                                  
             MVC   WKPRINT+20(96),0(R1)                                         
             LA    R2,OUTDCB                                                    
             LA    R0,WKPRINT                                                   
             PUT   (R2),(R0)                                                    
           ENDIF                                                                
         ENDIF                                                                  
*                                                                               
         BRCT  R11,LOADB20     ---LOOP                                          
*                                                                               
LOADB99  EQU   *                                                                
         L     R15,WKRETC                                                       
         BR    R10                                                              
*                                                                               
***********************************************************************         
*   RETURN NEXT "BUFSIZE" buffer                                      *         
***********************************************************************         
UNLOADB  DS    0H                                                               
         LLGT  R15,=F'96'                                                       
         MS    R15,WKBUFCNT                                                     
*                                                                               
         LLGT  R14,GPBLKSIZ                                                     
         USING GENBLKSZ,R14                                                     
         ST    R15,GP_RESULT_BLK_SIZE                                           
         DROP  R14                                                              
*                                                                               
         LA    R1,WKREC                                                         
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
         DROP  R4 GLOBAREA                                                      
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
         MVC   GPRECLEN,=F'96'                                                  
         MVC   GPRECMAX,=F'96'                                                  
         MVC   GPBLKMAX,=F'9600'                                                
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
*  LOAD  DYNAMIC SUBROUTINES                                          *         
***********************************************************************         
         LOAD  EP=GVBTP90         SEQUENTIAL  FILE  I/O          @I1015         
         Oilh  R0,MODE31                                         @I1015         
         ST    R0,GVBTP90                                        @I1015         
         wto 'GVBTP90 LOADED'                                                   
*                                                                @I1015         
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
         MVC   WKPRINT(32),=CL32'GVBXR7: Being executed with DD:'               
         MVC   WKPRINT+32(8),WKDDNAME                                           
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                @I1015         
*      echo THRD#                                                               
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(33),=CL33'GVBXR7: Being executed in thread:'             
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
         MVC   WKPRINT(32),=CL32'GVBXR7: Total number partitions:'              
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
         MVC   WKPRINT(33),=CL33'GVBXR7: Being executed with VIEW:'             
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
         MVC   WKPRINT(34),=CL34'GVBXR7: Being executed with LFID:'             
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
         MVC   WKPRINT(38),=CL38'GVBXR7: Being executed with MR95ENVV:'         
         MVC   WKPRINT+38(32),WKENVV                                            
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                @I1015         
*      echo DATETIME                                                            
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(33),=CL33'GVBXR7: Being executed DateTime:'              
         MVC   WKPRINT+33(16),WKDATETIME                                        
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                @I1015         
*      echo GP_STARTUP_DATA                                                     
         MVC   WKPRINT,SPACES                                                   
         MVC   WKPRINT(37),=CL37'GVBXR7: Being executed with startup:'          
         MVC   WKPRINT+37(32),WKSTART                                           
         LA    R2,OUTDCB                                                        
         LA    R0,WKPRINT                                                       
         PUT   (R2),(R0)                                                        
*                                                                               
***********************************************************************         
*  OPEN INPUT FILE                                                    *         
***********************************************************************         
         LA    R0,TP90AREA                                                      
         ST    R0,TP90PA                                                        
         LA    R0,WKREC           RETURN DATA HERE                              
         ST    R0,TP90RECA                                                      
         LA    R0,WKKEY           THIS IS REALLY THE RBA FOR ESDS               
         ST    R0,TP90KEYA                                                      
*                                                                               
         MVC   PAFUNC,=CL2'OP'    INITIALIZE FUNCTION CODE                      
         MVC   PADDNAME,WKDDNAME             DDNAME                             
         MVI   PAFTYPE,C'S'       FILE TYPE                                     
         MVC   PAFMODE,=CL2'I '   FILE MODE                                     
         MVI   PARECFMT,C'F'      FORMAT                                        
         LA    R1,TP90LIST        POINT R1 TO PARAMETER LIST                    
*        L     R15,=V(GVBTP90)    LOAD  ADDRESS OF "GVBTP90"                    
         L     R15,GVBTP90        LOAD  ADDRESS OF "GVBTP90"                    
         BASR  R14,R15            CALL  I/O ROUTINE                             
*                                                                               
         IF (LTR,R15,R15,NZ)                                                    
           MVC   WKPRINT,SPACES                                                 
           MVC   WKPRINT(30),=CL30'GVBXR7: Open Error xxxx/xxxx X'              
           MVC   WKPRINT+29(1),PARTNC                                           
           XGR   R15,R15                                                        
           ICM   R15,B'0001',PAVSAMRC+1                                         
           CVD   R15,WKDBLWK                                                    
           MVC   WKPRINT+19(4),NUMMSK+8                                         
           MVI   WKPRINT+19,C' '                                                
           ED    WKPRINT+19(4),WKDBLWK+6                                        
           ICM   R15,B'0001',PAVSAMRC                                           
           CVD   R15,WKDBLWK                                                    
           MVC   WKPRINT+24(4),NUMMSK+8                                         
           MVI   WKPRINT+24,C' '                                                
           ED    WKPRINT+24(4),WKDBLWK+6                                        
           LA    R2,OUTDCB                                                      
           LA    R0,WKPRINT                                                     
           PUT   (R2),(R0)                                                      
           MVC   WKRETC,=F'16'                                                  
           J     I0099                                                          
         ELSE                                                                   
           MVC   WKPRINT,SPACES                                                 
           MVC   WKPRINT(26),=CL26'GVBXR7: INPUT FILE OPENED: '                 
           MVC   WKPRINT+27(8),WKDDNAME                                         
           LA    R2,OUTDCB                                                      
           LA    R0,WKPRINT                                                     
           PUT   (R2),(R0)                                                      
         ENDIF                                                                  
*                                                                               
***********************************************************************         
* ENQ FOR DEALING WITH SHAREAD GLOBAL AREA                            *         
***********************************************************************         
INITENQ  MVC   REENTWK(MDLENQXL),MDLENQX                                        
         ENQ   MF=(E,REENTWK)                                                   
         LTR   R15,R15                                                          
         JZ    INITGLOB                                                         
         WTO 'GVBXR7: GLOBAL AREA ENQ FAILED'                                   
         MVC   WKRETC,=F'16'                                                    
         J     I0099                                                            
*                                                                               
***********************************************************************         
* CHECK FOR PRE-EXISTENCE OF GLOBAL AREA                              *         
***********************************************************************         
INITGLOB EQU   *                                                                
         MVC   WKTOKNAM+0(8),GENEVA                                             
         MVC   WKTOKNAM+8(8),TKNNAME                                            
         CALL  IEANTRT,(TOKNLVL2,WKTOKNAM,WKTOKN,WKTOKNRC),            X        
               MF=(E,REENTWK)                                                   
         LTGF  R15,WKTOKNRC       SUCCESSFUL   ???                              
         JNZ   INITGLBL           NO  - CREATE THE FIRST ONE                    
         LLGT  R4,WKTOKNGB        RETRIEVE  GLOBAL AREA  ADDRESS                
         ST    R4,WKGLOBA                                                       
         USING GLOBAREA,R4                                                      
         J     INITDEQ                                                          
         DROP  R4 GLOBAREA                                                      
***********************************************************************         
* INITIALIZE GLOBAL AREA MEMORY                                       *         
***********************************************************************         
INITGLBL EQU   *                                                                
         XC    WKTOKN,WKTOKN                                                    
         LHI   R0,GLOBLEN+8                                                     
         GETMAIN RU,LV=(0),LOC=(ANY)                                            
         MVC   0(8,R1),GLOBEYEB                                                 
         AHI   R1,8                                                             
         ST    R1,WKGLOBA                                                       
         ST    R1,WKTOKNGB                                                      
         LR    R4,R1                                                            
         USING GLOBAREA,R4                                                      
         XC    0(GLOBLEN,R4),0(R4)  ZERO GLOBAL AREA                            
         MVC   GLOPARTT,WKPARTS#    SET TOTAL NUMBER PARTITIONS                 
         DROP  R4 GLOBAREA                                                      
***********************************************************************         
*  CREATE GLOBAL NAME/TOKEN AREA                                      *         
***********************************************************************         
         CALL  IEANTCR,(TOKNLVL2,WKTOKNAM,WKTOKN,TOKNPERS,WKTOKNRC),   +        
               MF=(E,REENTWK)                                                   
         LTGF  R15,WKTOKNRC       SUCCESSFUL  ???                               
         JZ    INITDEQ                                                          
         WTO 'GVBXR7: GLOBAL AREA CREATE FAILED'                                
         MVC   WKRETC,=F'16'                                                    
***********************************************************************         
* DEQ FOR DEALING WITH SHAREAD GLOBAL AREA                            *         
***********************************************************************         
INITDEQ  MVC   REENTWK(MDLDEQXL),MDLDEQX                                        
         DEQ   MF=(E,REENTWK)                                                   
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
OP       DC    CL2'OP'                                                          
CL       DC    CL2'CL'                                                          
TOKNPERS DC    F'0'                    TOKEN PERSISTENCE                        
TOKNLVL1 DC    A(1)                    NAME/TOKEN  AVAILABILITY  LEVEL          
TOKNLVL2 DC    A(2)                    NAME/TOKEN  AVAILABILITY  LEVEL          
GLOBEYEB DC    CL8'GLOBAREA'                                                    
GVBXR7EY DC    CL8'GVBXR7'                                                      
GENEVA   DC    CL8'GENEVA'                                                      
TKNNAME  DC    CL8'GVBXRGB'                                                     
LPGMNAME DS   0CL8                     MINOR  ENQ  NAME (PROGRAM)               
EXTNAME  DC    CL128'GVBXRG'           MINOR  ENQ  NAME (EXTENSION)             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*        D A T A   C O N T R O L   B L O C K S                        *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
outfile  DCB   DSORG=PS,DDNAME=SYSPRINT,MACRF=(PM),DCBE=outfdcbe,      x        
               RECFM=FB,LRECL=131                                               
outfile0 EQU   *-outfile                                                        
outfdcbe DCBE  RMODE31=BUFF                                                     
outfilel EQU   *-outfile                                                        
*                                                                               
RPTFILE  DCB   DSORG=PS,DDNAME=MERGRPT,MACRF=(PM),DCBE=RPTDCBE,        X        
               RECFM=FBA,LRECL=133                                              
RPTEOFF  EQU   *-RPTFILE                                                        
RPTDCBE  DCBE  RMODE31=BUFF                                                     
RPTFILEL EQU   *-RPTFILE                                                        
*                                                                               
MDLENQP  ENQ   (GENEVA,LPGMNAME,E,,STEP),RNL=NO,MF=L                            
MDLENQPL EQU   *-MDLENQP                                                        
*                                                                               
MDLDEQP  DEQ   (GENEVA,LPGMNAME,,STEP),RNL=NO,MF=L                              
MDLDEQPL EQU   *-MDLDEQP                                                        
*                                                                               
MDLENQX  ENQ   (GENEVA,EXTNAME,E,,STEP),RNL=NO,MF=L                             
MDLENQXL EQU   *-MDLENQX                                                        
*                                                                               
MDLDEQX  DEQ   (GENEVA,EXTNAME,,STEP),RNL=NO,MF=L                               
MDLDEQXL EQU   *-MDLDEQX                                                        
*                                                                               
MDLWTO   WTO   TEXT=(R2),MF=L     MODEL WTO TO DISPLAY CONSOLE MESSAGES         
*                                                                               
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
*                    0 1 2 3 4 5 6 7 8 9 A B C D E F                            
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
