**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2008, 2021.
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
*  MODULE DESCRIPTION:                                                *
*                                                                     *
*      - THIS MODULE WORKS WITH THE GENEVA READ EXIT "GVBXRCK".       *
*                                                                     *
*      - THIS MODULE USES THE DIRECTORY BUILT BY "GVBXRCK" TO RETURN  *
*        ADDITIONAL INFORMATION RELATED TO THE CURRENT EVENT RECORD.  *
*                                                                     *
*      - WHEN CALLED, A POINTER CONTAINING THE ADDRESS OF THE         *
*        "CLOSEST" MATCHING "LOOK-UP" RECORD IS RETURNED.             *
*                                                                     *
*      - THE "CLOSEST" MATCHING RECORD IS THE ONE WITH THE CLOSEST    *
*        TIMESTAMP/EFFECTIVE DATE THAT IS LESS THAN OR EQUAL TO THE   *
*        REQUESTED TIMESTAMP/EFFECTIVE DATE.                          *
*                                                                     *
*      - A RETURN CODE INDICATES IF THE REQUESTED RECORD COULD NOT    *
*        BE LOCATED.                                                  *
*                                                                     *
*                                                                     *
*  GENEVA MODULES USED    : NONE                                      *
*                                                                     *
***********************************************************************
                        EJECT
***********************************************************************
*                                                                     *
*           MODULE RETURN CODES AND REGISTER DOCUMENTATION            *
*                                                                     *
***********************************************************************
*                                                                     *
*                                                                     *
*  RETURN CODES:                                                      *
*                                                                     *
*            0  - FOUND                                               *
*            4  - NOT FOUND                                           *
*            8  -                                                     *
*           12  - DISABLE VIEW                                        *
*           16  - ABEND   JOB                                         *
*                                                                     *
*  PARAMETERS:                                                        *
*                                                                     *
*        R1:  PARAMETER LIST ADDRESS                                  *
*                                                                     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*  REGISTER USAGE:                                                    *
*                                                                     *
*        R15 - TEMPORARY WORK    REGISTER                             *
*            - RETURN    CODE                                         *
*                                                                     *
*        R14 - TEMPORARY WORK    REGISTER                             *
*            - RETURN    ADDR                                         *
*                                                                     *
*        R13 - WORK AREA ADDRESS                                      *
*                                                                     *
*        R12 -                                                        *
*        R11 - PROGRAM    BASE   REGISTER                             *
*                                                                     *
*        R10 - SUBROUTINE CALL   RETURN   ADDRESS (1ST LEVEL)         *
*                                                                     *
*        R9  - CURRENT LOOK-UP   RECORD   POINTER  ADDRESS            *
*                                                                     *
*        R8  - PARAMETER LIST    ADDRESS                              *
*                                                                     *
*        R7  - EVENT     RECORD  ADDRESS (CURRENT  SOURCE)            *
*        R6  - LOOK-UP   KEY     ADDRESS                              *
*        R5  - SHARED DIRECTORY  ADDRESS (MIN/MAX)                    *
*                                                                     *
*        R4  - MATCHING  RECORD  ADDRESS (LOOK-UP  RESULTS)           *
*                                                                     *
*        R3  - EFFECTIVE DATE    LOOK-UP  CLOSEST  RECORD             *
*                                                                     *
*        R2  - MATCH      KEY    LENGTH   (-1)                        *
*                                                                     *
*        R1  - TEMPORARY WORK    REGISTER                             *
*            - PARAMETER LIST    ADDRESS             (UPON ENTRY)     *
*                                                                     *
*        R0  - TEMPORARY WORK    REGISTER                             *
*                                                                     *
***********************************************************************
                        EJECT
***********************************************************************
*                                                                     *
*       "GVBXLCK" - W O R K A R E A   D E F I N I T I O N             *
*                                                                     *
***********************************************************************
*
WORKAREA DSECT

         DS    xl(savf4sa_len)    Format 4 save area
*
DBLWORK  DS    D                  DOUBLEWORD  WORK AREA
*
LKUPENT  DS    CL08               LOGICAL     RECORD  ENTITY ID
*
DIRADDR  DS    A                  LOOK-UP DIRECTORY    ENTRY ADDRESS
INITADDR DS    A                  INITIALIZATION   RECORD    ADDRESS
*
FULLKEYL DS    F                  FILE    KEY LENGTH    (-1)
LKUPKEYL DS    F                  LOOK-UP KEY LENGTH    (-1)
LKUPKEYO DS    F                  LOOK-UP KEY OFFSET
TSTMPOFF DS    F                  LOOK-UP KEY TIMESTAMP      OFFSET
EFFDTOFF DS    F                  LOOK-UP KEY EFFECTIVE DATE OFFSET
*
SRCHADDR DS    A                  REQUESTED SEARCH   LOGIC  ADDRESS
*
PREVFND  DS    FD                 PREVIOUS LOOK-UP   ENTRY  ADDRESS
LKUPSTMP DS    XL10               CURRENT  LOOK-UP   KEY    TIMESTAMP
TSTMPCLO DS    XL10               CLOSEST  TIMESTAMP
*
LKUPEFFD DS    XL04               CURRENT  LOOK-UP   KEY    EFFECTIVE
EFFDTCLO DS    XL04               CLOSEST  EFFECTIVE DATE
*
EVNTADDR DS    A                  EVENT    RECORD    ADDRESS
EVNTTMSO DS    HL02               EVENT    TIMESTAMP OFFSET
EVNTREC# DS    PL06               EVENT    RECORD    NUMBER
*
LKUPKEY  DS    CL256              PREVIOUS LOOK-UP   KEY
*
INITWRKA DS    A                  RECORD INITIALIZATION WORK  AREA
*
PARMAREA DS   4Ad                 NAME/TOKEN PARAMETER LIST
*
TOKNRTNC DS    A                  NAME/TOKEN SERVICES  RETURN CODE
TOKNNAME DS    XL16               TOKEN NAME
TOKEN    DS   0XL16               TOKEN
TOKNBUFR DS    A                  SHARED    LR BUFFER ADDRESS
TOKNDIR  DS    A                  SHARED    DIRECTORY ADDRESS
TOKNFILE DS    A                  FILE AREA ADDRESS
TOKNPART DS    A                  PARTITION DATA      ADDRESS
*
INITIND  DS    CL01               EVENT INITIALIZATION INDICATOR
*
LKUPOPT  DS    XL01               LOOK-UP OPTIONS (TIMESTAMP, EFF DATE)
LKPTSTMP EQU   X'80'              TIMESTAMPS  PRESENT
LKPEFFDT EQU   X'40'              EFFECTIVE   DATES  PRESENT ???
*
NOTFOPT  DS    CL01               NOT FOUND  OPTION
         DS    XL01               (RESERVED)
*
WKSEQOVR DS    HL02               SEQUENTIAL SEARCH  OVERRIDE
*
WKTXTLEN DS    HL02               WTO  MESSAGE TEXT  LENGTH
WKPRINT  DS    CL128
*
WKRECCPY DS    CL8192             COPY OF EXTENSION  RECORD (IF NEEDED)
WORKLEN  EQU   *-WORKAREA
*                                                                     *
***********************************************************************
         IHASAVER DSECT=YES,SAVER=NO,SAVF4SA=YES,TITLE=NO
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        GENEVA PROVIDED PARAMETER LIST                               *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         copy    gvbx95pa
***********************************************************************
*                                                                     *
*        S T A R T U P   D A T A  (S T A T I C   T A R G E T   L R)   *
*                                                                     *
***********************************************************************
*
PARMDATA DSECT
*
PDENTTYP DS    CL08               REQUESTED    LOOK-UP  ENTITY  ID
PDKEYLEN DS    CL03               KEY LENGTH   OVERRIDE
PDKEYPOS DS    CL04               KEY POSITION OVERRIDE
PDNOTFOV DS    CL01               NOT FOUND    OVERRIDE
PDSEQOVR DS    CL02               SEQUENTIAL   SEARCH OVERRIDE
         DS    CL14
PDDATALN EQU   *-PARMDATA
                        EJECT
***********************************************************************
*                                                                     *
*        S H A R E D   D I R E C T O R Y   (P O I N T E R S)          *
*                                                                     *
***********************************************************************
*
         copy  dirarea            Directory area dsect
                        EJECT
***********************************************************************
*                                                                     *
*        G L O B A L   D A T A   A R E A                              *
*                                                                     *
***********************************************************************
*
         copy  globarea           xrck/xlck global area dsect
*
***********************************************************************
*                                                                     *
*        P A R T I T I O N   D A T A   A R E A                        *
*                                                                     *
***********************************************************************
*
         copy  partarea           Partition area dsect
                        EJECT
***********************************************************************
*                                                                     *
*        E N T I T Y  /  S O U R C E   D A T A   F I L E   A R E A    *
*                                                                     *
***********************************************************************
*
         copy  filearea           File area mapping DSECT
*
***********************************************************************
*                                                                     *
*        R E C O R D   I N I T I A L I Z A T I O N   F I L E          *
*                                                                     *
***********************************************************************
*
         copy RECINIT             RECORD  Init Dsect
                        EJECT
***********************************************************************
*                                                                     *
*        E V E N T   R E C O R D   P R E F I X                        *
*                                                                     *
***********************************************************************
*
EVNTREC  DSECT
*
EVNTRDW  DS   0XL04               RDW
         DS    HL02
         DS    XL02
EVNTENT  DS    CL08               EVENT ENTITY ID
EVNTPART DS    CL02               EVENT PARTITION
EVNTRSRC DS    CL01               EVENT TIME   PARTITION  ID
EVNTDATA DS   0CL01               EVENT RECORD
*
                        EJECT
***********************************************************************
*                                                                     *
*        L O O K - U P   K E Y   D E F I N I T I O N                  *
*                                                                     *
***********************************************************************
*
KEYDEFN  DSECT
*
KYLRID   DS    FL04               LOGICAL RECORD ID
KYSRCHCD DS    CL01               SEARCH  TYPE   CODE
KYDATA   DS   0CL01               KEY     DATA
*
                        EJECT
***********************************************************************
*                                                                     *
*        REGISTER EQUATES:                                            *
*                                                                     *
***********************************************************************
         YREGS
*
*
*
                        EJECT
         print off,nogen,noprint
         SYSSTATE ARCHLVL=2,amode64=YES
         COPY  ASMMSP
LEAVE    OPSYN ASM_LEAVE
         asmmrel on
         PRINT GEN,on
*
GVBXLCK  RMODE 24
GVBXLCK  AMODE 64
GVBXLCK  CSECT
         j     code
XLCKEYE  GVBEYE GVBXLCK
static   loctr            set up the static loctr
code     loctr            followed by the code loctr
prev     using savf4sa,r13          map the save area
         stmg  R14,R12,prev.SAVF4SAG64RS14 Save callers registers
         larl  r12,gvbxlck
         USING (GVBXLCK,code),R12
*
         LA    R8,0(,R1)          LOAD  PARAMETER LIST ADDRESS
         USING GENPARM,R8
*
         llgt  R9,GPWORKA         LOAD  WORK  AREA ANCHOR ADDR
         llgt  r14,gpenva         get enviroment info area ptr
         using genenv,r14           and map
         if    (cli,gpphase,eq,c'C')   close phase
*          then we dont have r1 pointing at a save area at this time
*          (unstructured and messy, but really need to clean up calling
*           sequence so that normal returns can be used)
           LHI R15,8           set return code
           BRU closexit           split
         elseif (lt,r1,0(,r9),np) any WA allocated
*
***********************************************************************
*  ALLOCATE "GVBXLCK" WORKAREA IF NOT ALREADY ALLOCATED (PER THREAD)  *
***********************************************************************
*
           LHI R0,WORKLEN+8       LOAD   WORK AREA SIZE
           STORAGE OBTAIN,LENGTH=(0),LOC=(ANY),CHECKZERO=YES GET WORKA
           if  cij,r15,ne,x'14'   not zeroed?
             Lgr  r10,r1            save address
             LgR  R0,R1             ZERO  WORK  AREA
             LghI R1,WORKLEN+8
             SgR  R14,R14
             SgR  R15,R15
             MVCL R0,R14
             lgr r1,r10            restore pointer
           endif
*
           MVC     0(l'workeyeb,R1),WORKEYEB
           aghi r1,l'workeyeb     move pointer past
           USING workarea,R1
           using savf4sa,workarea
           stg r13,savf4saprev     save old save area in current
           stg r1,prev.savf4sanext save current save area in old
           mvc savf4said(4),=a(savf4said_value) set 'F4SA' in area
           lgr R13,r1             Get new workarea into r13
           ST  R13,0(,R9)         SAVE  WORK  AREA ADDRESS
*                                                                     *
*        If SNAP is required, use SNAP=Y so that MR95 allocates and   *
*        opens the file.                                              *
*        The SNAPDCB address is saved in SNAPDCBA                     *
*                                                                     *
           BRAS R10,INITWORK      INITIALIZE WORK   AREA
*
*          LA  R9,36-1(,R8)
*          ly  r2,SNAPDCBA
*          SNAP DCB=(r2),ID=100,STORAGE=((R8),(R9))
*          LA  R9,WORKLEN-1(,R13)
*          SNAP DCB=(r2),PDATA=(REGS),ID=101,STORAGE=((R13),(R9))
*
           else
***********************************************************************
*    CHAIN REGISTER SAVE AREAS TOGETHER                               *
*    CHECK FOR CHANGE IN EVENT RECORD ADDRESS                         *
***********************************************************************
             stg   r13,savf4saprev     set   forward  pointer in old
             stg   r1,prev.savf4sanext set   backward pointer in new
             lgr   r13,r1           get new workarea into r13
             drop r1
             drop prev
             using workarea,r13
             using savf4sa,workarea
*
         endif
***********************************************************************
*  CHECK FOR OPEN or close phASE, RETURN RC=8 IF SO                   *
***********************************************************************
         llgt  R14,GPENVA         LOAD ENVIRONMENT INFO ADDRESS
         USING GENENV,R14
*
         if CLI,GPPHASE,eq,c'O'       OPEN PHASE ???
           j   return             yes - then exit                pgc15
         endif
*
         DROP  R14
*
         LLGT  R7,GPEVENTA        Load pointer to Event Record address
         LG    R7,0(,R7)          Initialize Event Record Base Register
         AGHI  R7,-4              BACKUP TO RDW
         USING EVNTREC,R7
*
         llgt  R6,GPKEYA          LOAD  LOOK-UP   KEY     ADDRESS
         USING KEYDEFN,R6
*
         if CLI,INITIND,ne,C'Y'   EVENT ENTITY INITIALIZATION needed ?
*
           jas R10,EVNTINIT       INITIALIZE EVENT RECORD ATTRIBUTES

           select cli,kysrchcd,eq
           when c'E'
             LArl R15,EFFDSRCH    EFFECTIVE  DATE LOOK-UP ???

           when c'B'
             larl R15,BINSRCH     BINARY SEARCH   ???

           when c'X'
             larl R15,STMPLOOP    EXACT TIMESTAMP LOOK-UP ???

           when c'R'
             larl R15,REVRSRCH    REVERSE SCAN    LOOK-UP ???

           when c'P'
             larl R15,PREVLOOP    PREVIOUS RECORD LOOK-UP ???

           when c'N'
             larl R15,NEXTLOOP    NEXT  RECORD    LOOK-UP ???

           when c'C'
             larl R15,COMBSRCH    COMBINATION     LOOK-UP ???

           othrwise
             larl R15,BUFRLOOP    DEFAULT         LOOK-UP

           endsel

           ST  R15,SRCHADDR

         endif
*
*        LA    R9,WORKLEN-1(,R13)
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=(REGS),ID=102,STORAGE=((R13),(R9))
*
         llgt  R5,GPFILEA
         USING GENFILE,R5
*
                        EJECT
***********************************************************************
*  COPY TIMESTAMP FROM EVENT RECORD                                   *
*  SAVE LOOK-UP   KEY                                                 *
***********************************************************************
COPYSTMP LGH   R1,EVNTTMSO        LOAD   TIMESTAMP OFFSET WITHIN EVENT
         LTgr  R1,R1
         BRNP  SAVEADDR
*
         LA    R1,EVNTDATA(R1)    SAVE   TIMESTAMP FROM   EVENT  REC
         MVC   LKUPSTMP,0(R1)
*
SAVEADDR ST    R7,EVNTADDR        UPDATE CURRENT   EVENT  RECORD ADDR
*
         LGF   R2,LKUPKEYL        SAVE   CURRENT   LOOK-UP  KEY
         exrl  R2,SAVEKEY
*
         DROP  R5
         DROP  R7
                        SPACE 3
***********************************************************************
*  SEARCH LOGICAL RECORD BUFFER BUILT FOR THIS ENTITY                 *
***********************************************************************
BUFRSRCH XC    PREVFND,PREVFND    RESET PREVIOUS LOOK-UP ENTRY ADDRESS
*
         Llgt  R5,DIRADDR         LOAD  RECORD   LIST  ADDRESS
*
         SGR   R3,R3              RESET CLOSEST  MATCH POINTER
*
         llgt  R9,0(,R5)          LOAD  POINTER  LIST  ADDRESS
*
*        L     R15,LKUPKEYL
*        LA    R15,5(R6,R15)
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),ID=110,STORAGE=((R6),(R15))
*
         Llgt  R15,SRCHADDR       BRANCH TO  REQUESED  SEARCH TYPE
         BR    R15
*
         DS   0D
SAVEKEY  MVC   LKUPKEY(0),KYDATA  * * * * *  E X E C U T E D  * * * * *
                        EJECT
***********************************************************************
*  LOOP THROUGH RECORDS FOR THE TARGET LOOK-UP RECORD TYPE            *
***********************************************************************
*        ORG   *,32
BUFRLOOP C     R9,4(,R5)          REACHED END OF LOOK-UP ENT TYPE ???
         BRNL  BUFRLAST           YES -  BRANCH
*
         LG    R4,0(,R9)          LOAD   STARTING  LOOK-UP RECORD ADDR
         USING EVNTREC,R4
*
*        LH    R15,0(,R4)         LOAD   LENGTH
*        AR    R15,R4
*        BCTR  R15,0
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=(REGS),ID=115,STORAGE=((R4),(R15))
*
***********************************************************************
*  SEARCH FOR A MATCH ON PARTIAL KEY EXCLUDING TIMESTAMP              *
*  IF SPECIFIC SOURCE INDICATED THEN FILTER CANDIDATE RECORDS         *
***********************************************************************
BUFRMAT  ltgf  R14,LKUPKEYO       LOAD TARGET KEY ADDRESS
         LA    R14,EVNTDATA-EVNTREC(R4,R14)
         BRNM  BUFRSRTD
*
BUFRUSRT exrl  R2,MATCH           PARTIAL KEY MATCH (UNSORTED KEYS)
         BRE   BUFRSTMP
         BRU   BUFRNEXT
*
BUFRSRTD exrl  R2,MATCH           PARTIAL KEY MATCH   (SORTED KEYS)
         BRH   BUFRLAST
         BRNE  BUFRNEXT
*
BUFRSTMP ltgf  R14,TSTMPOFF       MATCHING  TIMESTAMPS ??
         BRNP  BUFRFND
*
         LA    R14,EVNTDATA-EVNTREC(R4,R14)
         CLC   0(L'LKUPSTMP,R14),LKUPSTMP
         BRH   BUFRNEXT           NO  - TOO RECENT
*
         LTGR  R3,R3              AT LEAST ONE  CANDIDATE  FOUND ???
         BRP   BUFRCLOS           YES - IS THIS ONE BETTER
*
         MVC   TSTMPCLO,0(R14)
*
BUFRFRST LGR   R3,R4              POINT TO CURRENT RECORD
*
         BRU   BUFRNEXT
*
*
***********************************************************************
*  CHECK IF THIS LOOK-UP RECORD HAS CLOSER DATE/TIME                  *
***********************************************************************
BUFRCLOS CLC   0(L'LKUPSTMP,R14),TSTMPCLO CLOSER   DATE  ???
         BRL   BUFRNEXT           NO   -  BYPASS
*
         MVC   TSTMPCLO,0(R14)    UPDATE  CLOSEST  TIMESTAMP
         LGR   R3,R4              POINT   TO CURRENT  RECORD
*
BUFRNEXT LA    R9,8(,R9)          ADVANCE TO  NEXT RECORD
         BRU   BUFRLOOP           LOOP   UNTIL MAX REACHED
*
BUFRLAST LTGR  R4,R3              CLOSEST MATCH FOUND  ???
         BRNP  BUFRNOT            NO  - NOTHING TO  RETURN
*
BUFRFND  Llgt  R14,TOKNPART       YES - SET RECORD POINTER
         USING PARTAREA,R14
         agsi  PARTFNDT,bin1
         DROP  R14
*
         BRU   FOUND
*
BUFRNOT  Llgt  R14,TOKNPART
         USING PARTAREA,R14
         agsi  PARTNOTT,bin1
         DROP  R14
*
         BRU   NOTFOUND
                        EJECT
***********************************************************************
*  LOOP THROUGH RECORDS FOR THE TARGET LOOK-UP RECORD TYPE            *
***********************************************************************
*        ORG   *,32
EFFDSRCH LA    R15,KYDATA+1(R2)   LOAD EFF DT OFFSET (KEY)
         MVC   LKUPEFFD,0(R15)    SAVE EFF DT
*
EFFDLOOP C     R9,4(,R5)          REACHED END OF LOOK-UP ENT TYPE ???
         BRNL  EFFDLAST           YES -  BRANCH
*
         LG    R4,0(,R9)          LOAD   STARTING  LOOK-UP RECORD ADDR
*
*        LH    R15,0(,R4)         LOAD   LENGTH
*        AR    R15,R4
*        BCTR  R15,0
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),ID=115,STORAGE=((R4),(R15))
*
***********************************************************************
*  SEARCH FOR A MATCH ON PARTIAL KEY EXCLUDING EFFECTIVE DATE         *
*  IF SPECIFIC SOURCE INDICATED THEN FILTER CANDIDATE RECORDS         *
***********************************************************************
EFFDMAT  LGF   R14,LKUPKEYO       LOAD TARGET KEY ADDRESS
         LA    R14,EVNTDATA-EVNTREC(R4,R14)
         exrl  R2,MATCH           PARTIAL KEY MATCH   ???
         BRNE  EFFDNEXT           NO  -   ADVANCE
*
EFFDDATE LGF   R14,EFFDTOFF       LOAD EFF DT OFFSET (TARGET)
         LA    R14,EVNTDATA-EVNTREC(R4,R14)
         CLC   0(4,R14),LKUPEFFD  MATCHING EFFECTIVE DATES   ???
         BRH   EFFDNEXT           NO  - TOO RECENT
*
         LTGR  R3,R3              FIRST QUALIFYING TIMESTAMP ???
         BRP   EFFDCLOS           NO  - CHECK   IF CLOSER
*
         LGR   R3,R4              POINT TO CURRENT RECORD
         MVC   EFFDTCLO,0(R14)
*
         BRU   EFFDNEXT
*
*
***********************************************************************
*  CHECK IF THIS LOOK-UP RECORD HAS CLOSER DATE/TIME                  *
***********************************************************************
EFFDCLOS CLC   0(L'EFFDTCLO,R14),EFFDTCLO CLOSER   DATE  ???
         BRL   EFFDNEXT           NO   -  BYPASS
         LGR   R3,R4              POINT   TO CURRENT  RECORD
         MVC   EFFDTCLO,0(R14)    UPDATE  CLOSEST  TIMESTAMP
*
EFFDNEXT LA    R9,8(,R9)          ADVANCE TO  NEXT RECORD
         BRU   EFFDLOOP           LOOP   UNTIL MAX REACHED
                        SPACE 3
EFFDLAST LTGR  R4,R3              CLOSEST MATCH FOUND  ???
         BRNP  EFFDNOT            YES - SET RECORD POINTER
*
EFFDFND  Llgt  R14,TOKNPART
         USING PARTAREA,R14
         agsi  PARTFNDE,bin1
         DROP  R14
*
         BRU   FOUND
*
EFFDNOT  Llgt  R14,TOKNPART       NO  - NOTHING TO  RETURN
         USING PARTAREA,R14
         agsi  PARTNOTE,bin1
         DROP  R14
*
         BRU   NOTFOUND
                        EJECT
***********************************************************************
*  LOOP THROUGH RECORDS FOR THE TARGET LOOK-UP RECORD TYPE            *
***********************************************************************
*        ORG   *,32
STMPLOOP C     R9,4(,R5)          REACHED END OF LOOK-UP ENT TYPE ???
         BRNL  STMPLAST           YES -  BRANCH
*
         LG    R4,0(,R9)          LOAD   STARTING  LOOK-UP RECORD ADDR
*
*        LH    R15,0(,R4)         LOAD   LENGTH
*        AR    R15,R4
*        BCTR  R15,0
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),ID=115,STORAGE=((R4),(R15))
*
***********************************************************************
*  SEARCH FOR AN EXACT MATCH ON TIMESTAMP                             *
***********************************************************************
STMPDATE LGF   R14,TSTMPOFF       LOAD  TARGET KEY ADDRESS
         LA    R14,EVNTDATA-EVNTREC(R4,R14)
         CLC   0(L'LKUPSTMP,R14),LKUPSTMP MATCHING TIMESTAMP ???
         BRNE  STMPNEXT           NO  - TOO RECENT
*
         LGR   R3,R4              FIRST QUALIFYING TIMESTAMP ???
         BRU   STMPLAST
*
STMPNEXT LA    R9,8(,R9)          ADVANCE TO  NEXT RECORD
         BRU   STMPLOOP           LOOP   UNTIL MAX REACHED
                        SPACE 3
STMPLAST LTGR  R4,R3              CLOSEST MATCH FOUND  ???
         BRNP  STMPNOT            YES - SET RECORD POINTER
*
STMPFND  Llgt  R14,TOKNPART
         USING PARTAREA,R14
         agsi  PARTFNDX,bin1
         DROP  R14
*
         BRU   FOUND
*
STMPNOT  Llgt  R14,TOKNPART       NO  - NOTHING TO  RETURN
         USING PARTAREA,R14
         agsi  PARTNOTX,bin1
         DROP  R14
*
         BRU   NOTFOUND
                        EJECT
***********************************************************************
*  LOOP THROUGH RECORDS FOR THE TARGET LOOK-UP RECORD TYPE            *
***********************************************************************
PREVLOOP C     R9,4(,R5)          REACHED END OF LOOK-UP ENT TYPE ???
         BRNL  PREVLAST           YES -  BRANCH
*
         LG    R4,0(,R9)          LOAD   STARTING  LOOK-UP RECORD ADDR
*
*        LH    R15,0(,R4)         LOAD   LENGTH
*        AR    R15,R4
*        BCTR  R15,0
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),ID=115,STORAGE=((R4),(R15))
*
***********************************************************************
*  SEARCH FOR CLOSEST EFFECTIVE DATE LESS THAN KEY EFFECTIVE DATE     *
***********************************************************************
PREVDATE LGF   R14,EFFDTOFF       LOAD EFF DT OFFSET (TARGET)
         LA    R14,EVNTDATA-EVNTREC(R4,R14)
         LA    R15,KYDATA+1(R2)   LOAD EFF DT OFFSET (KEY)
         CLC   0(4,R14),0(R15)    MATCHING EFFECTIVE DATES   ???
         BRNL  PREVNEXT           NO  - TOO RECENT
*
         LTGR  R3,R3              FIRST QUALIFYING TIMESTAMP ???
         BRP   PREVCLOS           NO  - CHECK   IF CLOSER
*
         LGR   R3,R4              POINT TO CURRENT RECORD
         MVC   EFFDTCLO,0(R14)
*
         BRU   PREVNEXT
*
*
***********************************************************************
*  CHECK IF THIS LOOK-UP RECORD HAS CLOSER DATE/TIME                  *
***********************************************************************
PREVCLOS CLC   0(L'EFFDTCLO,R14),EFFDTCLO CLOSER   DATE  ???
         BRL   PREVNEXT           NO   -  BYPASS
         LGR   R3,R4              POINT   TO CURRENT  RECORD
         MVC   EFFDTCLO,0(R14)    UPDATE  CLOSEST  TIMESTAMP
*
PREVNEXT LA    R9,8(,R9)          ADVANCE TO  NEXT RECORD
         BRU   PREVLOOP           LOOP   UNTIL MAX REACHED
                        SPACE 3
PREVLAST LTGR  R4,R3              CLOSEST MATCH FOUND  ???
         BRNP  PREVNOT            YES - SET RECORD POINTER
*
         Llgt  R14,TOKNPART
         USING PARTAREA,R14
         agsi  PARTFNDE,bin1
         DROP  R14
*
         BRU   FOUND
*
PREVNOT  Llgt  R14,TOKNPART       NO  - NOTHING TO  RETURN
         USING PARTAREA,R14
         agsi  PARTNOTE,bin1
         DROP  R14
*
         BRU   NOTFOUND
                        EJECT
***********************************************************************
*  LOOP THROUGH RECORDS FOR THE TARGET LOOK-UP RECORD TYPE            *
***********************************************************************
NEXTLOOP C     R9,4(,R5)          REACHED END OF LOOK-UP ENT TYPE ???
         BRNL  NEXTLAST           YES -  BRANCH
*
         LG    R4,0(,R9)          LOAD   STARTING  LOOK-UP RECORD ADDR
*
*        LH    R15,0(,R4)         LOAD   LENGTH
*        AR    R15,R4
*        BCTR  R15,0
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),ID=115,STORAGE=((R4),(R15))
*
***********************************************************************
*  SEARCH FOR CLOSEST EFFECTIVE DATE GREATER THAN KEY EFFECTIVE DATE  *
***********************************************************************
NEXTDATE LGF   R14,EFFDTOFF       LOAD EFF DT OFFSET (TARGET)
         LA    R14,EVNTDATA-EVNTREC(R4,R14)
         LA    R15,KYDATA+1(R2)   LOAD EFF DT OFFSET (KEY)
         CLC   0(4,R14),0(R15)    MATCHING EFFECTIVE DATES   ???
         BRNH  NEXTNEXT           NO  - TOO RECENT
*
         LTGR  R3,R3              FIRST QUALIFYING TIMESTAMP ???
         BRP   NEXTCLOS           NO  - CHECK   IF CLOSER
*
         LGR   R3,R4              POINT TO CURRENT RECORD
         MVC   EFFDTCLO,0(R14)
*
         BRU   NEXTNEXT
*
*
***********************************************************************
*  CHECK IF THIS LOOK-UP RECORD HAS CLOSER DATE/TIME                  *
***********************************************************************
NEXTCLOS CLC   0(L'EFFDTCLO,R14),EFFDTCLO CLOSER   DATE  ???
         BRNL  NEXTNEXT           NO   -  BYPASS
         LGR   R3,R4              POINT   TO CURRENT  RECORD
         MVC   EFFDTCLO,0(R14)    UPDATE  CLOSEST  TIMESTAMP
*
NEXTNEXT LA    R9,8(,R9)          ADVANCE TO  NEXT RECORD
         BRU   NEXTLOOP           LOOP   UNTIL MAX REACHED
                        SPACE 3
NEXTLAST LTGR  R4,R3              CLOSEST MATCH FOUND  ???
         BRNP  NEXTNOT            YES - SET RECORD POINTER
*
NEXTFND  Llgt  R14,TOKNPART
         USING PARTAREA,R14
         agsi  PARTFNDE,bin1
         DROP  R14
*
         BRU   FOUND
*
NEXTNOT  Llgt  R14,TOKNPART       NO  - NOTHING TO  RETURN
         USING PARTAREA,R14
         agsi  PARTNOTE,bin1
         DROP  R14
*
         BRU   NOTFOUND
                        EJECT
***********************************************************************
*  BINARY SEARCH THROUGH ENTITY RECORDS FOR MATCHING KEY              *
*                                                                     *
*        R2: KEY    LENGTH    (-1)                                    *
*        R3: BOTTOM SUBSCRIPT (0)                                     *
*        R5: DIRECTORY  ENTRY ADDRESS                                 *
*        R9: FIRST     RECORD POINTER ADDRESS                         *
*                                                                     *
***********************************************************************
*        ORG   *,32
COMBSRCH LLGT  R0,4(,R5)          COMPUTE LEN OF DIRECTORY
         SGR   R0,R9
         SRL   R0,3               DIVIDE  BY  8 (SIZE OF EACH POINTER)
         CH    R0,WKSEQOVR
         BRL   BUFRLOOP           CHANGE BINARY TO SEQUENTIAL
*
BINSRCH  LA    R15,KYDATA+1(R2)   LOAD EFF DT OFFSET (KEY)
         MVC   LKUPEFFD,0(R15)    SAVE EFFECTIVE DATE (IF ANY)
*
         LGF   R2,LKUPKEYL        LOAD  LOOKUP KEY LENGTH (-1)
         LA    R6,LKUPKEY-5       POINT TO SAVED KEY WITH X'00' SUFFIX
*
         LGF   R5,4(,R5)          LOAD    ENDING  POINTER ADDRESS
         AGHI  R5,-8
         SGR   R5,R9              COMPUTE POINTER LIST    LENGTH (-8)
         BRM   BINSNOT            BYPASS  SEARCH  IF NO  RECORDS
*
         SRLG  R5,r5,3            DIVIDE  BY 8  (TOP SUBSCRIPT) (N-1)
*
BINSLOOP CGR   R3,R5              BOTTOM  EXCEEDS TOP ???
         BRH   BINSLAST           YES  -  SEARCHING  FINISHED
*
         LA    R10,0(R3,R5)       COMPUTE  MIDPOINT
         SRLG  R10,R10,1          DIVIDE   BY 2
*
         LGR   R0,R10             SAVE     MIDPOINT SUBSCRIPT
*
         SLLG  R10,R10,3          MULTIPLY BY 8
         AGR   R10,R9             ADD BASE
*
         LG    R4,0(,R10)         LOAD  LOOK-UP RECORD   ADDR
         USING EVNTREC,R4
*
*        LH    R15,0(,R4)         LOAD  LENGTH
*        AR    R15,R4
*        BCTR  R15,0
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),ID=115,STORAGE=((R4),(R15))
*
***********************************************************************
*  COMPARE LOOK-UP AND RECORD KEYS                                    *
***********************************************************************
         LGF   R14,LKUPKEYO       LOAD TARGET KEY ADDRESS
         LA    R14,EVNTDATA-EVNTREC(R4,R14)
*
         exrl  R2,MATCH           COMPARE KEYS (INCL TIMESTAMP)
         BRL   BINSBOT            RAISE BOTTOM POINTER
         BRH   BINSTOP            LOWER TOP    POINTER
*
*
***********************************************************************
*  COMPARE EFFECTIVE DATES  (IF ANY)                                  *
***********************************************************************
         LGF   R1,EFFDTOFF        EFFECTIVE DATE OFFSET
         LTgr  R1,R1
         BRNP  BINTSTMP
*
         LA    R1,EVNTDATA-EVNTREC(R4,R1)
         CLC   0(4,R1),LKUPEFFD
         BRL   BINSBOT            RAISE BOTTOM POINTER
         BRH   BINSTOP            LOWER TOP    POINTER
         BRU   BINSFND            EXACT MATCH
*
*
***********************************************************************
*  COMPARE TIMESTAMPS  (IF ANY)                                       *
***********************************************************************
BINTSTMP LGF   R1,TSTMPOFF        EFFECTIVE DATE OFFSET
         LTgr  R1,R1
         BRNP  BINSFND
*
         LA    R1,EVNTDATA-EVNTREC(R4,R1)
         CLC   0(L'LKUPSTMP,R1),LKUPSTMP
         BRL   BINSBOT            RAISE BOTTOM POINTER
         BRH   BINSTOP            LOWER TOP    POINTER
         BRU   BINSFND            EXACT MATCH
*
*
***********************************************************************
*  LOWER THE TOP    POINTER       (USE BOTTOM HALF OF ENTITY)         *
***********************************************************************
BINSTOP  LGR   R5,R0              LOAD ADDRESS   OF CURR   VALUE NODE
         BCTgr R5,0               DECREMENT BY   1
         BRU   BINSLOOP           YES - CONTINUE
*
***********************************************************************
*  RAISE THE BOTTOM POINTER       (USE TOP    HALF OF ENTITY)         *
***********************************************************************
BINSBOT  LGR   R3,R0              LOAD ADDRESS   OF HIGHER VALUE NODE
         AGHI  R3,1               INCREMENT BY   1
         BRU   BINSLOOP           YES - CONTINUE
*
*
***********************************************************************
*  CHECK IF THIS LOOK-UP RECORD HAS SAME ROOT KEY EXCLUDING DATE      *
***********************************************************************
BINSLAST TM    LKUPOPT,LKPTSTMP+LKPEFFDT  DATES SPECIFIED ???
         BRZ   BINSNOT            NO  - NOTHING FOUND
*
         LGF   R2,LKUPKEYL        LOAD  SUPPLIED  KEY  LENGTH
         exrl  R2,MATCH           RECHECK LAST  ENTRY EXAMINED (ROOT)
         JH    BINSLOWR           LAST RECORD > KEY (CHECK ONE LOWER)
         JL    BINSXFER
*
         C     R2,FULLKEYL        FULL KEY PROVIDED  ???
         BRNE  BINSXFER           NO  - TRANSFER TO SEQUENTIAL SEARCH
*
*
***********************************************************************
*  COMPARE LAST EFFECTIVE DATE   (IF ANY)                             *
***********************************************************************
         TM    LKUPOPT,LKPEFFDT   EFFECTIVE DATE PRESENT
         BRNO  BINLSTTS
*
         CLC   0(4,R1),0(R15)
         BRH   BINSLOWR           EFFECTIVE DATE GREATER THAN KEY DATE
         BRU   BINSFND            EFFECTIVE DATE LESS THAN OR EQUAL
*
*
***********************************************************************
*  COMPARE LAST TIMESTAMP                                             *
***********************************************************************
BINLSTTS CLC   0(L'LKUPSTMP,R1),LKUPSTMP
         BRH   BINSLOWR           TIMESTAMP GREATER THAN KEY DATE
         BRU   BINSFND            TIMESTAMP LESS THAN OR EQUAL
                        SPACE 3
***********************************************************************
*  BACK DOWN TO LOWER KEYED RECORD                                    *
***********************************************************************
BINSLOWR AGHI  R10,-8             BACKUP  TO PREVIOUS RECORD POINTER
*
         CGR   R10,R9             CHECK FOR BEGINNING OF TABLE  ???
         JL    BINSXFER           EXIT  IF  NO PREVIOUS  ENTRY
*
         LG    R4,0(,R10)         LOAD  PREVIOUS RECORD  ADDRESS
*
         LGF   R14,LKUPKEYO       LOAD  TARGET KEY ADDR
         LA    R14,EVNTDATA-EVNTREC(R4,R14)
*
         exrl  R2,MATCH           COMPARE ROOT KEY
         JNE   BINSXFER
*
         C     R2,FULLKEYL        FULL KEY PROVIDED  ???
         BRNE  BINSXFER           NO  - TRANSFER TO SEQUENTIAL SEARCH
                        SPACE 3
***********************************************************************
*  MATCHING KEY     FOUND                                             *
***********************************************************************
BINSFND  Llgt  R14,TOKNPART       YES - SET RECORD POINTER
         USING PARTAREA,R14
         agsi  PARTFNDB,bin1
         DROP  R14
*
         BRU   FOUND
*
***********************************************************************
*  MATCHING KEY NOT FOUND                                             *
***********************************************************************
*        IF (CLC,=CL3'RAL',EQ,LKUPENT)
*          DC  H'0'
*        ENDIF
BINSNOT  DS    0H
         Llgt  R14,TOKNPART
         USING PARTAREA,R14
         agsi  PARTNOTB,bin1
         DROP  R14
*
         J     NOTFOUND
*
BINSXFER LGF   R5,DIRADDR        RELOAD DIRECTORY ADDRESS
         LGR   R9,R10            SET    CURRENT   POINTER   ADDRESS
         SGR   R3,R3             RESET  CLOSEST   MATCH
         BRU   EFFDMAT           TRANSFER TO EFFECTIVE DATE SEQ SRCH
*
         DS   0D
MATCH    CLC   0(0,R14),KYDATA   * * * *  E X E C U T E D  * * * *
*
                        EJECT
*
***********************************************************************
*  LOOP BACKWARDS THROUGH RECORDS FOR THE TARGET LOOK-UP RECORD KEY   *
***********************************************************************
*        ORG   *,32
REVRSRCH LGF   R9,4(,R5)          LOAD LAST RECORD POINTER ADDR (+1)
         AGHI  R9,-8
*
REVRLOOP C     R9,0(,R5)          REACHED BEG OF RECORD POINTER LIST ??
         BRL   REVRNOT            YES -  NO  QUALIFYING RECORDS
*
         LG    R4,0(,R9)          LOAD   LOOK-UP RECORD ADDR
         USING EVNTREC,R4
*
*        LH    R15,0(,R4)         LOAD   LENGTH
*        AR    R15,R4
*        BCTR  R15,0
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),ID=115,STORAGE=((R4),(R15))
*
***********************************************************************
*  SEARCH FOR A MATCH ON PARTIAL KEY EXCLUDING TIMESTAMP              *
***********************************************************************
REVRMAT  LGF   R14,LKUPKEYO       LOAD TARGET KEY ADDRESS
         LA    R14,EVNTDATA-EVNTREC(R4,R14)
         exrl  R2,MATCH           PARTIAL KEY MATCH ???
         BRL   REVRNOT            GIVE UP SEARCH IF EVENT KEY < LOOKUP
         BRNE  REVRNEXT
*
REVRSTMP LGF   R14,TSTMPOFF       MATCHING TIMESTAMPS ???
         LTgr  R14,R14
         BRNP  REVRFND
*
         LA    R14,EVNTDATA-EVNTREC(R4,R14)
         CLC   0(L'LKUPSTMP,R14),LKUPSTMP
         BRNH  REVRFND
*
***********************************************************************
*  SCAN BACKWARDS TO PREVIOUS LOOK-UP RECORD                          *
***********************************************************************
REVRNEXT AGHI  R9,-8              BACK-UP TO  PREV RECORD  POINTER
         BRU   REVRLOOP           LOOP   UNTIL BEG REACHED
                        SPACE 3
***********************************************************************
*  SCAN FINISHED                                                      *
***********************************************************************
REVRFND  Llgt  R14,TOKNPART       YES - SET RECORD POINTER
         USING PARTAREA,R14
         agsi  PARTFNDR,bin1
         DROP  R14
*
         BRU   FOUND
*
REVRNOT  Llgt  R14,TOKNPART
         USING PARTAREA,R14
         agsi  PARTNOTR,bin1
         DROP  R14
*
         BRU   NOTFOUND
*
         DROP  R4
                        EJECT
***********************************************************************
*  RETURN A POINTER TO THE ELEMENT FOUND                              *
***********************************************************************
FOUND    STG   R4,PREVFND         SAVE PREVIOUS  ENTRY FOUND ADDRESS
*
         Llgt  R14,GPBLOCKA       PASS ELEMENT ADDRESS TO CALLER
         LA    R0,4(,R4)          SKIP RDW
         stg   R0,0(,R14)
*
*        LH    R15,0(,R4)
*        AR    R15,R4
*        BCTR  R15,0
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),ID=121,STORAGE=((R4),(R15))
*
***********************************************************************
*  RETURN TO CALLER (GENEVA)                                          *
***********************************************************************
RETURN   SGR   R15,R15            ZERO RETURN CODE
*
RETURNRC L     R14,GPRTNCA        LOAD RETURN CODE  ADDRESS
         ST    R15,0(,R14)
*
         lg    r13,savf4saprev         restore caller save area
closexit lg    r14,savf4sag64rs14      restore caller's r14
         lmg   r2,r12,savf4sag64rs2    restore caller's r2- r12
         BSM   0,R14              RETURN
*
*
RTNABEND Llgt  R2,GPENVA          LOAD   ENVIRONMENT INFO   ADDRESS
         USING GENENV,R2
*
         LH    R15,0(,R1)         RETURN TEXT LENGTH
         ST    R15,GP_ERROR_BUFFER_LEN
*
         BCTR  R15,0              DECREMENT   LENGTH FOR  "EX"
         Llgt  R14,GP_ERROR_BUFFER_PTR ERROR MESSAGE BUFFER ADDRESS
         exrl  R15,ERRCOPY
*
RTNAB16  LA    R15,16             SET RETURN CODE  REGISTER TO 16
         ST    R15,GP_ERROR_reason
         j     Returnrc
*
         DROP  R2

ERRCOPY  MVC   0(0,R14),2(R1)     * * * * *  E X E C U T E D  * * * * *

*
***********************************************************************
*  MATCHING ELEMENT    NOT FOUND (RETURN INVALID POINTER)             *
*  SET RETURN  CODE TO NOT FOUND                                      *
***********************************************************************
NOTFOUND Llgt  R14,TOKNPART       YES - SET RECORD POINTER
         USING PARTAREA,R14
         Llgt  R14,PARTGLOB       LOAD GLOBAL PARAMETER AREA ADDRESS
         USING GLOBAREA,R14
*
         CLI   NOTFOPT,C'C'       CONTINUE ???
         BRE   NOTFCONT           YES - RETURN  WITH   "NOT FOUND" RC
         DROP  R14
*
         Llgt  R2,GPENVA          LOAD   ENVIRONMENT INFO   ADDRESS
         USING GENENV,R2
*
         LGF   R10,LKUPKEYL       LOAD LOOK-UP  KEY    LENGTH (-1)
         AGHI  R10,L'TSTMPCLO+L'EFFDTCLO
*
         LAY   R1,MSGNOTF         LOAD MESSAGE  PREFIX LENGTH
         LGH   R15,0(,R1)
         LA    R0,1(R15,R10)      LOAD ADJUSTED MSG    LENGTH INCL KEY
         ST    R0,GP_ERROR_BUFFER_LEN
*
         Lgf   R14,GP_ERROR_BUFFER_ptr ERROR MESSAGE BUFFER ADDRESS
         aghi  R15,-1             DECREMENT   LENGTH FOR  "EX"
         exrl  R15,ERRCOPY
*
         LA    R14,1(R14,R15)
         exrl  R10,NOTFKEY         COPY LOOK-UP  KEY TO END
*
         LGHI  R15,4              SET RETURN CODE  REGISTER TO 4
         ST    R15,GP_ERROR_reason
*
         Llgt  R14,TOKNPART       YES - SET RECORD POINTER
         USING PARTAREA,R14
         Llgt  R14,PARTGLOB       LOAD GLOBAL PARAMETER AREA ADDRESS
         USING GLOBAREA,R14
*
         CLI   NOTFOPT,C'M'       MESSAGE ONLY ???
         BRE   NOTFCONT           YES - CONTINUE
*
         CLI   NOTFOPT,C'D'       DUMP  ???
         BRNE  RTNAB16            NO  - BYPASS S0C4 ABEND
         DROP  R14
*
         ABEND 804,DUMP
*
         DROP  R2
*
NOTFCONT LGF   R14,GPBLOCKA
         LLGTR R14,R14
         MVC   0(8,R14),HEXFF     64 bit address
         XC    PREVFND,PREVFND
*
         LGHI  R15,4              SET RC=4 (NOT  FOUND)
         BRU   Returnrc           RETURN
*
MSGCOPY  MVC   WKTXTLEN(0),0(R14) * * * * *  E X E C U T E D  * * * * *
NOTFKEY  MVC   0(0,R14),LKUPKEY   * * * * *  E X E C U T E D  * * * * *
                        EJECT
***********************************************************************
*                                                                     *
*        I N I T I A L I Z E   W O R K   A R E A                      *
*                                                                     *
***********************************************************************
         USING WORKAREA,R13
*
INITWORK ds    0h
*
         MVC   LKUPKEYL,HEX8F     INITIALIZE KEY LENGTH (NOT SPECIFIED)
         MVC   LKUPKEYO,HEX8F     INITIALIZE KEY OFFSET (NOT SPECIFIED)
*
         MVC   TOKNNAME+0(l'GENEVA),geneva
         llgt  R14,GPFILEA
         USING GENFILE,R14
         MVC   TOKNNAME+8(8),GPDDNAME

         DROP  R14
*
         CALL  IEAN4RT,(TOKNLVL,TOKNNAME,TOKEN,TOKNRTNC),              X
               MF=(E,PARMAREA)
         lt    R15,TOKNRTNC       SUCCESSFUL  ???
         BRZ   INITTFND           YES - RETURN
*
         LAY   R1,MSGTOKN         DISPLAY "NAME/TOKEN" MESSAGE
         BRU   RTNABEND
*
INITTFND llgt  R1,GPSTARTA        LOAD STARTUP  DATA   ADDRESS
         USING PARMDATA,R1
*
         llgt  R14,TOKNPART       YES - SET RECORD POINTER
         USING PARTAREA,R14
         llgt  R14,PARTGLOB       LOAD GLOBAL PARAMETER AREA ADDRESS
         USING GLOBAREA,R14
*
         MVC   NOTFOPT,GLOBLKUP   SAVE DEFAULT NOT FOUND OPTION
*
         llgt  R14,TOKNFILE
         USING FILEAREA,R14
*
INITLKUP CLC   FILERTYP,HEXFF     END-OF-TABLE   ???
         BRE   INITERRL           YES - INDICATE ERROR
*
         CLC   PDENTTYP,FILERTYP  MATCHING   ENTITY   TYPE   ???
         BRE   INITLFND
         LA    R14,FILELEN(,R14)
         BRU   INITLKUP
*
INITERRL LAY   R14,MSGTARG        DISPLAY "STRUCTURE NOT FOUND" MESSAGE
         LH    R15,0(,R14)
         LA    R15,2-1(,R15)
         exrl  R15,MSGCOPY
         MVC   WKPRINT+40(8),PDENTTYP
         L     R14,GPENVA         LOAD ENVIRONMENT INFO ADDRESS
         USING GENENV,R14
         L     R15,GPVIEW#
         CVD   R15,DBLWORK        CONVERT SORT FIELD LENGTH TO DECIMAL
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         UNPK  WKPRINT+40+8+7(8),DBLWORK
*
         LA    R1,WKTXTLEN
         BRU   RTNABEND
         DROP  R14
*
*
***********************************************************************
*  SAVE TARGET ENTITY TYPE ATTRIBUTES                                 *
***********************************************************************
         USING FILEAREA,R14
INITLFND MVC   LKUPENT,FILERTYP   SAVE LOOK-UP  ENTITY ID   FROM TABLE
*
         CLC   PDKEYLEN,SPACES    KEY  LENGTH   OVERRIDE SPECIFIED ???
         BRE   INITKPOS
*
         PACK  DBLWORK,PDKEYLEN   CONVERT KEY   LENGTH TO OFFSET
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         CVB   R0,DBLWORK
*
         LTR   R0,R0
         BRNP  INITKPOS
         BCTR  R0,0
         ST    R0,LKUPKEYL
*
INITKPOS CLC   PDKEYPOS,SPACES    KEY  POSITION OVERRIDE SPECIFIED ???
         BRE   INITNOTF
*
         PACK  DBLWORK,PDKEYPOS   CONVERT KEY Position TO OFFSET
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         CVB   R0,DBLWORK
*
         BCTR  R0,0
         ST    R0,LKUPKEYO
*
INITNOTF CLC   PDNOTFOV,SPACES    NOT  FOUND    OVERRIDE SPECIFIED ???
         BRE   INITSOVR
         OC    PDNOTFOV,PDNOTFOV  NOT  FOUND    OVERRIDE SPECIFIED ???
         BRZ   INITSOVR
*
         MVC   NOTFOPT,PDNOTFOV   SAVE OVERRIDE
*
seqovrd  equ   20
INITSOVR LGHI  R0,SEQOVRD         DEFAULT THRESHOLD
         STH   R0,WKSEQOVR
*
         CLC   PDSEQOVR,SPACES    SEQUENTIAL SEARCH OVERRIDE  SPEC ???
         BRE   INITTMSO
         OC    PDSEQOVR,PDSEQOVR  NOT  FOUND    OVERRIDE SPECIFIED ???
         BRZ   INITTMSO
*
         PACK  DBLWORK,PDSEQOVR
         OI    DBLWORK+L'DBLWORK-1,X'0F'
         CVB   R0,DBLWORK
         STH   R0,WKSEQOVR
*
         DROP  R1
*
INITTMSO LH    R0,FILETMSO        SAVE TIMESTAMP   KEY OFFSET (-1)
         ST    R0,TSTMPOFF
         if (cij,r0,gt,0)         TIMESTAMPS PRESENT ???
           OI  LKUPOPT,LKPTSTMP   YES - SET FLAG
         endif
         LH    R0,FILEEFDO        SAVE EFFECTIVE  DATE OFFSET
         ST    R0,EFFDTOFF
         if (cij,r0,gt,0)         TIMESTAMPS PRESENT ???
           OI  LKUPOPT,LKPEFFDT   YES - SET FLAG
         endif
         CLC   LKUPKEYO,HEX8F     KEY  POS    OVERRIDE SPECIFIED ???
         BRNE  INITKEYL
*
         LH    R15,FILEKEYO       SAVE KEY OFFSET
         ST    R15,LKUPKEYO
*
INITKEYL LH    R15,FILEKEYL       SAVE KEY LENGTH (-1)
         BCTR  R15,0
         ST    R15,FULLKEYL
*
         CLC   LKUPKEYL,HEX8F     KEY  LENGTH OVERRIDE SPECIFIED ???
         BRNE  INITMINA
         ST    R15,LKUPKEYL
*
*
***********************************************************************
*  FILL-IN DIRECTORY ADDRESSES IN WORKAREA BASED ON TARGET FILE TYPE  *
***********************************************************************
INITMINA L     R0,FILEMINA
         ST    R0,DIRADDR
*
*
***********************************************************************
*  FILL-IN MATCHING INITIALIZATION RECORD ADDRESS                     *
***********************************************************************
         L     R0,FILEINIA
         ST    R0,INITADDR
*
         BR    R10                RETURN
*
                        EJECT
***********************************************************************
*                                                                     *
*  I N I T I A L I Z E   E V E N T   E N T I T Y   A T T R I B U T E S*
*                                                                     *
***********************************************************************
         USING EVNTREC,R7
*
EVNTINIT llgt  R14,TOKNFILE
         USING FILEAREA,R14
*
***********************************************************************
*  LOCATE MATCHING EVENT RECORD FILE DEFINITION                       *
***********************************************************************
EVNTLOOP CLC   FILERTYP,HEXFF     END-OF-TABLE   ???
         BRE   EVNTERRE           YES - INDICATE ERROR
*
         CLC   EVNTENT,FILERTYP   MATCHING   ENTITY   TYPE   ???
         BRE   EVNTFND
         LA    R14,FILELEN(,R14)
         BRU   EVNTLOOP
*
EVNTERRE ltgf  R0,TSTMPOFF        DOES TARGET ENTITY HAVE TIMESTAMPS ??
         BRNP  EVNTEXIT
*
         LAY   R14,MSGEVNT        DISPLAY "STRUCTURE NOT FOUND" MESSAGE
         LGH   R15,0(,R14)
         LA    R15,2-1(,R15)
         exrl  R15,MSGCOPY
         MVC   WKPRINT+39(8),EVNTENT
*
         LA    R1,WKTXTLEN
         BRU   RTNABEND
*
*
***********************************************************************
*  SAVE EVENT ENTITY TYPE ATTRIBUTES                                  *
***********************************************************************
EVNTFND  LGH   R0,FILETMSO        SAVE TIMESTAMP   KEY OFFSET (-1)
         STH   R0,EVNTTMSO
*
EVNTEXIT MVI   INITIND,C'Y'       SET  INITIALIZATION  INDICATOR
*
         BR    R10                RETURN
*
         DROP  R7
         DROP  R14
                        EJECT
static   loctr
***********************************************************************
*                                                                     *
*        C O N S T A N T S                                            *
*                                                                     *
***********************************************************************
*
bin1     equ   1
*
TOKNLVL  DC    A(1)                    NAME/TOKEN AVAILABILITY   LEVEL
GENEVA   DC    CL8'GENEVA'                  TOKEN NAME
*
WORKEYEB DC    CL8'XLCKWORK'
*
HEX8F    DC    XL04'8FFFFFFF'
HEXFF    DC  64X'FF'
SPACES   DC    CL128' '
*
         LTORG
*
*
MSGTOKN  DC    AL2(L'MSGTOKNT)
MSGTOKNT DC    C'GVBXLCK - CANNOT LOCATE "GVBXRCK" NAME/TOKEN'
*
MSGTARG  DC    AL2(L'MSGTARGT+L'MSGTARGV)
MSGTARGT DC    C'GVBXLCK - TARGET ENTITY NAME NOT FOUND: XXXXXXXX'
MSGTARGV DC    C' VIEW: XXXXXXXX'
*
MSGEVNT  DC    AL2(L'MSGEVNTT)
MSGEVNTT DC    C'GVBXLCK - EVENT ENTITY NAME NOT FOUND: XXXXXXXX'
*
MSGNOTF  DC    AL2(L'MSGNOTFT)
MSGNOTFT DC    C'GVBXLCK - LOOK-UP NOT FOUND: '
*
         ds    0h
GVBXLCK  CSECT
*
         END
