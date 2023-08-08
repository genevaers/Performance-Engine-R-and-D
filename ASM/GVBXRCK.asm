        TITLE 'GVBXRCK - READ/MERGE COMMON KEY ENTITY FILES'
**********************************************************************
*
* (C) COPYRIGHT IBM CORPORATION 2006, 2021.
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
*                         : THIS MODULE WILL READ AND MERGE RECORDS   *
*                           FROM THE DIFFERING ENTITIES FOR THE       *
*                           SAME BREAK ID INTO A MEMORY BUFFER        *
*                           (THE LOGICAL RECORD BUFFER) WHICH IS      *
*                           PASSED TO "GENEVA" ONE BREAK ID AT A      *
*                           TIME.                                     *
*                                                                     *
*                         : IF NECESSARY, MULTIPLE 32K (RECFM=VB      *
*                           FORMAT) BLOCKS WILL BE BUILT TO CONTAIN   *
*                           ALL THE RECORDS ASSOCIATED WITH ONE       *
*                           BREAK ID.                                 *
*                                                                     *
*                         : DURING ENTITY LOADING, DUPLICATE RECORDS  *
*                           ARE REDUCED OR ELIMINATED WHERE POSSIBLE  *
*                           DEPENDING ON THE COLLAPSE OPTION CODE.    *
*                           (ADDITIONAL DETAILS PROVIDED IN THE CODE) *
*                                                                     *
*                         : PROCESSING OCCURS IN THE FOLLOWING ORDER  *
*                              1) THE "LR BUFFER" INCLUDING EXTENSION *
*                                 (IF NEEDED) IS FILLED               *
*                              2) THE FIRST TRANSACTION BLOCK IS BUILT*
*                                 (IF THERE ARE ANY TRAN ENTITIES)    *
*                              3) THE "BUFFER READY" EXIT IS CALLED   *
*                                 (IF SPECIFIED)                      *
*                              4) THE FIRST "LR BUFFER" BLOCK IS      *
*                                 DELIVERED                           *
*                              5) GENERATED TRANSACTIONS (IF ANY)     *
*                                 ARE DELIVERED                       *
*                              6) THE NEXT  "LR BUFFER" BLOCK IS      *
*                                 DELIVERED                           *
*                              7) THE NEXT GENERATED TRANSACTIONS (IF *
*                                 ANY) ARE DELIVERED                  *
*                              8) AFTER THE "LR BUFFER" IS DELIVERED  *
*                                 TRANSACTION RECORDS ARE DELIVERED   *
*                                                                     *
*  GENEVA MODULES USED    : GVBUR35 - DYNAMIC FILE ALLOCATION         *
*                         : GVBUR30 - DB2   ACCESS                    *
*                         : GVBUR20 - SEQUENTIAL FILE  ACCESS         *
*                         : GENPIPE - READ  GENEVA PIPE         @017I *
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
*        R13 - REGISTER  SAVE AREA    ADDRESS (GVBXRCK WORK AREA)     *
*                                                                     *
*        R12 - PROGRAM   BASE REGISTER                                *
*        R11 - Work register                                          *
*                                                                     *
*        R10 - INTERNAL  SUBROUTINE   RETURN   ADDRESS                *
*                                                                     *
*        R9  - PARAMETER LIST         ADDRESS                         *
*            - OUTPUT    RECORD       ADDRESS                         *
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
*        "GVBXRCK"    W O R K   A R E A                               *
*                                                                     *
***********************************************************************
*
XRCKWORK DSECT
         ds    Xl(SAVF4SA_LEN)
*
WKSAVSUB DS  18fd                 INTERNAL  SUBROUTINE  REG  SAVE  AREA
callexit_r8save ds fd             save areafoor reg 8 when CALLEXIT is
*
WKMR95WA DS    A                 "GVBMR95"  WORK  AREA  ADDRESS
*
TCBTOKEN DS    XL16              "TTOKEN"
*
USERTOKN DS   2A                 "IARV64"   USER  TOKEN
*
WKTIMWRK DS   0XL16
WKDBLWRK DS    D                  TEMPORARY DOUBLEWORD  WORK AREA
WKDBLWK2 DS    D                  TEMPORARY DOUBLEWORD  WORK AREA
WKDBLWK3 DS    D                  TEMPORARY DOUBLEWORD  WORK AREA
*
WKPLISTA DS    A                  CALLER    PROVIDED    PARAMETER  LIST
*
WKVWLIST DS    A                  VIEW CHAIN ADDRESS (BREAKLEN=EXTRACT)
WKVWCNT  DS    F                  VIEW COUNT
WKVWDCMP DS    A                  DECOMPRESSED  VIEW    RECORD  ADDRESS
*
WKTIOTA  DS    A                 "TIOT"   EXTRACT ADDRESS
*
WKDDNAME DS    CL8                LOGICAL EVENT   FILE  DDNAME
WKLFID   DS    FL04               LOGICAL EVENT   FILE  LFID
*
WKENTCNT DS    F                 "PARMTBL" ENTITY ROW  COUNT
*
WKFILCNT DS    F                  FILE    CONTROL BLOCK AREA
WKFILSIZ DS    F
WKFILBEG DS    A
WKFILMAX DS    A
WKFILELO DS    A                  OPEN FILE LIST ENTRY  FOR LOWEST KEY
*
WKOPNCNT DS    F                  OPEN    FILE    LIST  AREA
WKOPNSIZ DS    F
WKOPNBEG DS    A
WKOPNMAX DS    A
WKOPNBRK DS    A                  CURRENT ENTITY BREAK  POINT IN LIST
WKOPNCUR DS    A                  CURRENT OPEN   FILE   BEING PROCESSED
*
WKRINITB DS    A                  RECORD  INITIALIZATION TABLE - BEG
WKRINITM DS    A                                               - MAX
WKRINITE DS    A                                               - END
*
WKDIRBEG DS    A                  SHARED  DIRECTORY AREA
WKDIROFF DS    A                  SHARED  DIRECTORY OFFSET/LENGTH
*
WKLSTSIZ DS    F                  LR BUFFER REC ADDR LIST  - SIZE @020I
WKLSTBEG DS    A                                           - BEG
WKLSTMAX DS    A                                           - MAX
WKLSTCUR DS    A                                           - CUR
WKLSTEND DS    A                                           - END
*
WKBUFBEG DS    FD                 BEG  OF OUTPUT  BUFFER
WKBUFEND DS    FD                 END  OF OUTPUT  DATA
WKBUFMAX DS    FD                 END  OF OUTPUT  BUFFER
WKBUFHI  DS    FD                "HIGHWATER MARK" (MAX  ADDRESS USED)
*
WKBLKBDW DS    FD                 CURRENT OUTPUT  BLOCK  BDW  ADDRESS
WKBLKEND DS    FD                 END  OF OUTPUT  BLOCK
WKBLKMAX DS    FD                 END  OF CURRENT BUFFER
WKBLKHI  DS    FD                "HIGHWATER MARK" (MAX  ADDRESS USED)
*
WKGENBEG DS    FD                 GENERATED  TRAN BEG
WKGENBDW DS    FD                             BDW PTR
WKGENEND DS    FD                            CURR PTR
WKGENMAX DS    FD                             MAX PTR
WKGENPBE DS    FD                             PRIMARY  BUFFER   END
*
WKTRNBDW DS    AD                 TRANSACTION BDW PTR
WKTRNEND DS    AD                 TRANSACTION END CUR
WKTRNMAX DS    AD                 TRANSACTION END MAX
*
*                                 TRANSACTION DELIVERY CONTINUATION
WKTCONT2 DS    A                      CURRENT OUTPUT  RECORD ADDR (R2)
WKTCONT3 DS    A                      CURRENT FILE BREAK ENT ADDR (R3)
WKTCONT5 DS    A                      CURRENT OPEN FILE LIST ENTRY(R5)
WKTCONT9 DS    AD                     CURRENT BUFFER ADDRESS      (R9)
WKTCONTA DS    A                      CURRENT RETURN ADDRESS      (R10)
*
WKPROSIZ DS    A                  TIMESTAMP PROFILE ID ASSIGN TBL - SIZ
WKPROBEG DS    A                                                  - BEG
WKPROMAX DS    A                                                  - MAX
WKPROEND DS    A                                                  - END
*
WKPROOLD DS    H                  PREVIOUS  PROFILE ID (REASSIGNED)
WKPRONEW DS    H                  NEWLY    ASSIGNED PROFILE   ID
WKPROSAV DS   (PROTBLEN)X         COLLECTED PROFILE ID TABLE  SAVE AREA
*
WKRECPRV DS    AD                 PREVIOUS  SAME ENTITY  RECORD ADDR
WKLSTREC DS    AD                 LAST RECORD IN LR BUFFER
WKSUBAVA DS    A                  AVAILABLE SUBSTITUTION RECORD
*
WKFILBRK DS    A                  FILE  TYPE BREAK  (BRK FILEAREA)
*
WKCALLEV DS    CL12               CLOSE EXIT CALL GENEVA ENV AREA(TEMP)
WKEXITRC DS    F                  LAST  BXIT CALL RETURN CODE
WKRXITRC DS    F                  LAST  RXIT CALL RETURN CODE
WKCALLPT DS    ad                 RETURNED RECORD POINTER
WKCALLRL DS    F                  RETURNED RECORD LENGTH          @008C
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
*
SVRUN#   DS    F                  RUN NUMBER
SVRUNDT  DS    CL08               RUN DATE
*
WKBEGTIM DS    F                  EXECUTION START TIME
WKBEGDAT DS    PL05                               DATE
*
WKFAILED DS    CL01               ONE OR MORE FILE     OPENS  FAILED
*
WKEXTUSE DS    CL01               CURRENT   THREAD     USING  EXT AREA
WKIGNRDD DS    CL01               IGNORE    MISSING    DDNAME ERR MSG
*
WKBRKLEN DS    HL02               KEY BREAK LENGTH
WKMAXKEY DS    HL02               MAXIMUM   KEY        LENGTH (-1)
WKOUTLEN DS   0HL02               CURRENT   OUTPUT     RECORD LENGTH
WKMAXLEN DS    HL02               MAXIMUM   FILE TYPE  RECORD LENGTH
*
WKTSTMP  DS    XL10               TIMESTAMP SAVE AREA
*
WKSAVLEN DS    F                  NEXT      LOWEST     BREAK  KEY LEN
WKBRKLOW DS    A                  NEXT      LOWEST     BREAK  ID
WKBRKID  DS    A                  CURRENT   BREAK      ID
WKBRKOLD DS    A                  PREVIOUS  BREAK      ID
*
WKOUTREC DS    A                  CURRENT   OUTPUT     RECORD
*
WKTOKNRC DS    A                  NAME/TOKEN  SERVICES RETURN CODE
WKTOKNAM DS    XL16               TOKEN NAME
WKTOKN   DS   0XL16               TOKEN
WKTOKNBA DS    A                  LR  BUFFER  ADDRESS
WKTOKNDA DS    A                  SHARED    DIRECTORY ADDRESS
WKTOKNFA DS    A                  FILE AREA ADDRESS
WKTOKNPA DS    A                  PARTITION DATA      ADDRESS
*
WKPARMA  DS    A                  PARAMETER TABLE     ADDRESS
WKGLOBA  DS    A                 "GVBXRCK"  GLOBAL    DATA    ADDRESS
*
GVBUR20  DS    A                 "GVBUR20" ADDRESS
GVBUR30  DS    A                 "GVBUR30" ADDRESS
GVBUR35  DS    A                 "GVBUR35" ADDRESS
GENPIPE  DS    A                 "GENPIPE" ADDRESS                @017I
*
WKUR20WA DS    A                 "GVBUR20" WORK AREA ANCHOR
*
WKBXITEP DS    A                  BUFFER   READY EXIT TRUE ENTRY POINT
WKBXITCA DS    A                  BUFFER   READY EXIT CALL ADDRESS
WKBXITAN DS    A                  BUFFER   READY EXIT ANCHOR
WKBXITPM DS    CL32               BUFFER   READY EXIT PARAMETERS
*
WKPIPEPL DS   0XL24               "GENPIPE" PARAMETER LIST        @017I
WKPIPEEA DS    A                     ENVIRONMENT AREA ADDR (CL?)  @017I
WKPIPEBA DS    A                     BLOCK    LENGTH  ADDR (HL2)  @017I
WKPIPEPA DS    A                     BLOCK    POINTER ADDR (AL4)  @017I
WKPIPEFA DS    A                     BLOCK    FORMAT  ADDR (CL1)  @017I
WKPIPELA DS    A                     "LRECL"          ADDR (HL2)  @017I
WKPIPERA DS    A                     RETURN   CODE    ADDR (HL2)  @017I
*
WKPIPERC DS    HL2                   RETURN   CODE                @017I
         DS    XL2                (RESERVED)                      @017I
*
WKRPTDCB DS    A                  CONTROL   REPORT      "DCB"
WKINIDCB DS    A                  RECORD INITIALIZATION "DCB"
WKPRMDCB DS    A                  PARAMETER FILE        "DCB"
WKVDPDCB DS    A                  VIEW DEFN FILE        "DCB"
*
WKPRTCNT DS    PL06               PARTITION RECORDS   READ
WKPRTDUP DS    PL06               PARTITION DUPLICATE RECORDS
WKPRTBYT DS    PL08               PARTITION BYTE      TOTAL
*
WKGENCNT DS    FD                 GENERATED TRANSACTION  RECORD COUNT
WKGENBYT DS    PL08               GENERATED TRANSACTION  BYTE   COUNT
*
WKEXTFLG DS    CL01               INPUT FILES IN GENEVA EXTRACT FORMAT
WKPADCMP DS    CL01               NUMERIC PAD DECOMPRESS OPTION ("CT")
*
WKBUFNO  DS    FL04               "BUFNO" FOR "GVBUR20"
*
         IARV64 MF=(L,GET64MEM)
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
***********************************************************************
*                                                                     *
*        MESSAGE  FORMATS                                             *
*                                                                     *
***********************************************************************
*
         ORG   WKPRTTXT
ERRORMSG DS   0CL133
ERRCC    DS    CL01' '
ERRFMTID DS    CL07' '
         DS    CL02' '
ERRKEY   DS    CL20' '
         DS    CL02' '
ERRTEXT  DS    CL50' '
         DS    CL54' '
*
         ORG   WKPRTTXT
HDR3MSG  DS   0CL133
         DS    CL17
HDR3DATE DS    CL08
         DS    CL98
*
         ORG   WKPRTTXT
HDR4MSG  DS   0CL133
         DS    CL17
HDR4TIME DS    CL08
         DS    CL98
*
         ORG   WKPRTTXT
STATPARM DS   0CL133
         DS    CL01
STATENT  DS    CL08
         DS    CL01
STATDDNP DS    CL08
         DS    CL01
STATRSRC DS    CL12
         DS    CL01
STATTPRT DS    CL05
         DS    CL02
STATETYP DS    CL01
         DS    CL02                                               @010C
STATCLAP DS    CL01
         DS    CL01                                               @010C
STATPFXL DS    CL02
         DS    CL01
STATKEYO DS    CL03
STATKEYL DS    CL03
STATBRKL DS    CL03                                               @010C
STATTMSO DS    CL03                                               @010C
         DS    CL01
STATEFDO DS    CL04                                               @010C
STATPROO DS    CL03                                               @010C
         DS    CL02
STATDB2  DS    CL01
         DS    CL03
STATGDG  DS    CL02
         DS    CL01
STATDSN  DS    CL44
         DS    CL01
STATMAP  DS    CL12
*
         ORG   WKPRTTXT
STATMSG  DS   0CL133
         DS    CL01
STATDDN  DS    CL08
         DS    CL01
STATRTYP DS    CL10
         DS    CL01
STATMSTI DS    CL15
         DS    CL05
STATMSTL DS    CL07
         DS    CL03
STATMSTB DS    CL20
         DS    CL06
STATMSTD DS    CL15
         DS    CL43
         ORG
*
WKJFCBEX DS    A
*
WKFILCT  DS    PL12               RTC19034 Counter for Rec Ct * RecL
*
WKTYPSW  DS    CL1                RTC19235 Flag for checking type order
*
WKKEYLN  DS    H                  RTC19235 for checking keylen < brklen
*
WKKEYSV1 DS    CL1024             KEY  SAVE AREA -1
WKKEYSV2 DS    CL1024             KEY  SAVE AREA -2
WKKEYSV3 DS    CL1024             KEY  SAVE AREA -3
*
WKJFCB   DS    CL176              JOB  FILE CONTROL  BLOCK
*
*
*        The following 2 areas are used for compatibility purposes for
*        the old input parameters of LRBUFFER and EXTENSION. These 2
*        parameters have been replaced by BUFSIZE and TBUFSIZE.
*        Previously LRBUFFER was below "BAR" storage for the inputed
*        records, included in this are was a 32k copy area for copying
*        an above the "bar" record to below the "bar" so we always
*        passed block back to GVBMR95 that was in 31 bit storage.
*        Also in this area was a 32k buffer for records from a file
*        type of "T". With GVBMR95 now running in 64 bit mode we have
*        now made LRBUFFER and EXTENSION into 1 buffer that resides
*        above the "bar" and this is set by the parameter BUFSIZE. The
*        "T" file type records are stored in a buffer set by the
*        TBUFSIZE parameter and is now a buffer by itself. For
*        compatibility if the user still specifies LRBUFFER and
*        EXTENSION but not BUFSIZE then BUFSIZE will be set to the
*        total of LRBUFFER and EXTENSION otherwise the values will be
*        ignored. If TBUFSIZE is not set then it will default to 1MB
*        in size. The following 2 fields are used to hold the LRBUFFER
*        and EXTENSION parameters incase BUFSZIE has not been specified
*
lrbsize  ds    f
lrxsize  ds    f
*
         DS   0D
HDRLIST  DS    XL(HEADERPR_L)
BUFFER   DS    CL1200             MESSAGE BUILD AREA
WKDYNAR  DS    (M35S99LN)C
*
WORKLEN  EQU   (*-XRCKWORK)
*
                        EJECT
***********************************************************************
*                                                                     *
*        P A R A M E T E R   C A R D     (O P E N   F I L E S)        *
*                                                                     *
***********************************************************************
*
CARDPARM DSECT                    ENTITIES AND  SOURCES TO BE OPENED
*
CARDENT  DS    CL08               ENTITY   ID
         DS    CL01
CARDKEYW DS    CL04               OPTIONAL PARAMETER KEYWORD
         DS    CL01
CARDDDN  DS    CL08               DDNAME   TEMPLATE
         DS    CL01
CARDRSRC DS    CL12               CATEGORIES
         DS    CL01
CARDPFR  DS    CL02               STARTING PARTITION ID
         DS    CL01
CARDPTO  DS    CL02               ENDING   PARTITION ID
         DS    CL01
CARDETYP DS    CL01               ENTITY   TYPE
         DS    CL01
CARDCLAP DS    CL01               COLLAPSE CODE
         DS    CL01
CARDPFXL DS    CL02               EMBEDDED PARTITION   ID LENGTH
*        DS    CL01
CARDKEYP DS    CL03               ENTITY   KEY   POSITION
*        DS    CL01
CARDKEYL DS    CL03               ENTITY   KEY   LENGTH
*        DS    CL01
CARDTMSP DS    CL03               TIMESTAMP      POSITION
*        DS    CL01
CARDEFDP DS    CL03               EFFECTIVE DATE POSITION
*        DS    CL01
CARDPROP DS    CL03               PROFILE  ID    POSITION
         DS    CL01
CARDDB2  DS    CL01               DB2      INDICATOR
         DS    CL01
CARDGDG  DS    CL02               GDG      INDICATOR (DEFAULT = 0)
                        SPACE 3
***********************************************************************
*                                                                     *
*        P A R A M E T E R   C A R D   D A T A S E T   N A M E        *
*                                                                     *
***********************************************************************
*
         ORG   CARDRSRC           ENTITIES AND  SOURCES TO BE OPENED
CARDDSN  DS    CL44               DATASET  NAME
*
         ORG   CARDRSRC           ENTITIES AND  SOURCES TO BE OPENED
CARDXNAM DS    CL08               EXIT     NAME
         DS    CL05              (RESERVED)
CARDXPRM DS    CL32               EXIT     PARAMETERS
*
         ORG   CARDRSRC           ENTITIES AND  SOURCES TO BE OPENED
CARDOMAP DS    CL12
*
         ORG
*
*
*
                        EJECT
***********************************************************************
*                                                                     *
*        P A R A M E T E R   T A B L E   (O P E N   F I L E S)        *
*                                                                     *
***********************************************************************
*
PARMTBL  DSECT                    ENTITIES AND  SOURCES TO BE OPENED
*
PARMENT  DS    CL08               ENTITY   ID
PARMETYP DS    CL01               ENTITY   TYPE
PARMCLAP DS    CL01               COLLAPSE CODE
PARMDDN  DS    CL08               DDNAME   TEMPLATE
PARMRSRC DS    CL12               RECORD   SOURCES  (CATEGORIES)
PARMPFR  DS    CL02               STARTING PARTITION ID
PARMPTO  DS    CL02               ENDING   PARTITION ID
PARMPFXL DS    HL02               EMBEDDED PARTITION ID   LENGTH
PARMKEYO DS    HL02               KEY      OFFSET
PARMKEYL DS    HL02               KEY      LENGTH
PARMBRKL DS    HL02               BREAK    LENGTH
PARMTMSO DS    HL02               TIMESTAMP OFFSET
PARMEFDO DS    HL02               EFFECTIVE DATE OFFSET
PARMPROO DS    HL02               PROFILE   ID   OFFSET
PARMPROA DS    AL04               PROFILE  ENTITY  PARM   ADDRESS
PARMDSN  DS    CL44               DATASET  NAME
PARMDB2  DS    CL01               DB2      INDICATOR (ALSO "EXCP")
PARMGDG  DS    CL02               GDG      INDICATOR (DEFAULT = 0)
PARMDCMP DS    XL01               ENTITY DECOMPPRESS
*                                                                 @008I
PARMKXIT DS    CL08               ENTITY KEY EXIT NAME            @008I
PARMKXEP DS    AL04               ENTITY KEY EXIT TRUE ENTRY POINT@008I
*
PARMRXIT DS    CL08               ENTITY READ  EXIT NAME
PARMRXEP DS    AL04               ENTITY READ  EXIT TRUE ENTRY POINT
PARMRXCA DS    AL04               ENTITY READ  EXIT CALL ADDRESS
PARMRXPM DS    CL32               ENTITY READ  EXIT CALL PARMS    @008C
*
PARMPXIT DS    CL08               PROFILE  ID  EXIT NAME
PARMPXEP DS    AL04               PROFILE  ID  EXIT TRUE ENTRY POINT
PARMPXCA DS    AL04               PROFILE  ID  EXIT CALL ADDRESS
PARMPXPM DS    CL32               PROFILE  ID  EXIT CALL ADDRESS
*
PARMOMAP DS    CL12               OUTPUT  FILE CATEGORY  MAP
PARMKMAP DS    CL12               LAST KEY VAL CATEGORY  MAP
*
PARMNEWB DS    CL01               ENTITY BEGINS A NEW LR BUFFER BLOCK
*
PARMDUPS DS    CL01               Y - Allow duplicate keys  N - error
*
         ds    0f                 make it a fullword boundary
PARMLEN  EQU   (*-PARMTBL)
*
***********************************************************************
*                                                                     *
*        R E C O R D   I N I T I A L I Z A T I O N   F I L E          *
*                                                                     *
***********************************************************************
*
         copy RECINIT             RECORD  Init Dsect
                        EJECT
*
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
*
***********************************************************************
*                                                                     *
*        E N T I T Y  /  S O U R C E   D A T A   F I L E   A R E A    *
*                                                                     *
***********************************************************************
*
         copy  filearea           File area mapping DSECT
                        EJECT
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
*        S H A R E D   D I R E C T O R Y   (P O I N T E R S)          *
*                                                                     *
***********************************************************************
         copy  dirarea            Directory area dsect
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        VIEW PARAMETERS                                              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
VIEWREC  DSECT                    VIEW   DEFN  RECORD
*
VWNEXT   DS    AL04               NEXT   VIEW  DEFN   RECORD
VWVIEW#  DS    FL04               VIEW   ID    NUMBER
*
VWCOLCNT DS    FL04               COLUMN   COUNT
VWMAXCOL DS    FL04               MAXIMUM  COLUMN#  ("CT" COLUMN)
VWMINCOL DS    FL04               MINIMUM  COLUMN#  ("CT" COLUMN)
*
VWRECLEN EQU   *-VIEWREC
                        EJECT
*
***********************************************************************
*                                                                     *
*        P R O F I L E   I D   A S S I G N M E N T   T A B L E        *
*                                                                     *
***********************************************************************
*
PROIDTBL DSECT
*
PROBRKID DS    CL48               BREAK  ID
PROTSTMP DS    XL10               TIMESTAMP
*
PROIDCUR DS    HL02               PROFILE   ID - CURRENT
PROIDOLD DS    HL02               PROFILE   ID - OLD
*
PROTBLEN EQU   *-PROIDTBL
                        EJECT
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
EXSORTKY DS   0CL01
                        SPACE 3
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        SUBTOTAL/SUMMARY EXTRACT COLUMN DEFINITION                   *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
COLEXTR  DSECT                 CALCULATION COLUMN DATA  AREA
*
COLNO    DS    HL02            CALCULATION COLUMN NUMBER
COLDATA  DS    PL12            CALCULATION DATA
*
COLDATAL EQU   *-COLEXTR       CALCULATION COLUMN DATA  LENGTH
                        EJECT
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
                        EJECT
***********************************************************************
*                                                                     *
*        I N T E R N A L   E N T R Y   P O I N T   W O R K   A R E A  *
*                                                                     *
***********************************************************************
*
EPWORKAR DSECT
*
         ds    xl(savf5sa_len)
EPTOKEN  DS   0CL32
EPTOKNAM DS    CL16
EPTOKN   DS   0XL16
EPTOKNBA DS    A                  LR  BUFFER  ADDRESS
EPTOKNDA DS    A                  SHARED    DIRECTORY ADDRESS
EPTOKNFA DS    A                  FILE      AREA      ADDRESS
EPTOKNPA DS    A                  PARTITION DATA AREA ADDRESS
*
EPTOKNRC DS    F
*
EPXRCKWK DS    A                 "GVBXRCK"  WORK AREA ADDRESS
*
EPENTID  DS    CL08               LAST ENTITY ID
EPDIRAD  DS    A                  LAST ENTITY DIRECTORY ENTRY ADDRESS
EPPTRAD  DS    A                  LAST ENTITY RECORD  POINTER ADDRESS
EPLASTRC DS    F                  LAST RETURN CODE
*
EPREENT  DS    CL32
*
EPWORKLN EQU   *-EPWORKAR
                        EJECT
***********************************************************************
*                                                                     *
*        L R   B U F F E R   E X T E N S I O N   B L O C K            *
*                                                                     *
***********************************************************************
*
                        EJECT
         COPY  GVBMR95W
                        EJECT
         COPY  GVB0001A
                        EJECT
         COPY  GVB0200A
                        EJECT
         COPY  GVB1000A
                        EJECT
         COPY  IEABRC
                        EJECT
         COPY  GVB2000A
                        EJECT
         copy  gvbmrzpe        ziip function equates
                        EJECT
         copy  gvbutequ        ziip function equates
                        EJECT
         print off
         SYSSTATE ARCHLVL=2
         COPY  ASMMSP
LEAVE    OPSYN ASM_LEAVE
         asmmrel on
         print on
*
* UR20PAREA  GVBUR20P pre=UR20P,dsect=y
***********************************************************************
*                                                                     *
*        D Y N A M I C   A L L O C A T I O N   P A R A M E T E R S    *
*                                                                     *
***********************************************************************
         MACRO
***********************************************************************
*        THIS STRUCTURE MUST BE INITIALIZED TO BINARY ZEROES.  THIS
*        MEANS 'LOW-VALUES' FOR COBOL PROGRAMMERS.
*
*        MEMBER NAME --> GVBAUR35
*        DSECT NAME  --> M35SVC99
***********************************************************************
*
&NAME    GVBAUR35 &DSECT=YES
         AIF   ('&DSECT' EQ 'YES').MUR3510
M35SVC99 DS    0D
         AGO   .MUR3520
.MUR3510 ANOP
M35SVC99 DSECT
.MUR3520 ANOP
M35FCODE DS    CL1      FUNCTION CODE
M35FCDAL EQU   C'1'      - ALLOCATE
M35FCDUN EQU   C'2'      - DEALLOCATE
.*
         DS    CL1      FILLER FOR ALIGNMENT
.*
M35RCODE DS    H        SVC99 RETURN CODE
.*
M35VLSEQ DS    H        VOLUME SEQUENCE NUMBER
.*
M35VLCNT DS    H        VOLUME COUNT
.*
         DS    CL70     RESERVED FOR FUTURE TEXT UNIT VALUES
.*
M35DDNAM DS    CL8      DATA DEFINITION NAME
.*
M35DSNAM DS    CL46     DATASET NAME
.*
M35MEMBR DS    CL8      MEMBER NAME OR RELATIVE GDG
.*
M35STATS DS    CL3      EXISTING DATASET STATUS
.*                      'OLD' 'MOD' 'NEW' 'SHR'
M35NDISP DS    CL7      NORMAL DISPOSITION
.*                      'UNCATLG' 'CATLG' 'DELETE' 'KEEP'
M35CDISP DS    CL7      CONDITIONAL DISPOSITION
.*                      'UNCATLG' 'CATLG' 'DELETE' 'KEEP'
M35VLSER DS    CL6      VOLUME SERIAL NUMBER
         DS    5CL6     RESERVED FOR MORE VOLSERS
.*
M35CLOSE DS    CL1      DATASET FREED AT CLOSE
.*                      'Y' 'N'
.*                      THE DEFAULT IS 'N'
M35RECFM DS    CL2      RECORD FORMAT
.*                      'FB' 'F ' 'VB' 'V ' 'FA' 'U '
         DS    CL316    RESERVED FOR FUTURE TEXT UNITS
.*
M35S99LN EQU   (*-M35SVC99)
         MEND
*
         GVBAUR35 DSECT=YES
                        EJECT
*
***********************************************************************
*                                                                     *
*        REGISTER EQUATES:                                            *
*                                                                     *
***********************************************************************
*
         YREGS
*
*
MAXPARM  EQU   60         MAXIMUM NUMBER OF POSSIBLE  PARAMETER ENTRIES
MAXPROID EQU   2000       MAXIMUM NUMBER OF PROFILE   ID  TABLE RECORDS
FILENBUF EQU   6          No. OF BUFFERS PER INPUT FILE (make it even)
*
                        EJECT
         PRINT nogen
*
GVBXRCK  RMODE 24
GVBXRCK  AMODE 31
GVBXRCK  CSECT
         J     CODE
XRCKEYE  GVBEYE GVBXRCK
*
static   loctr            set up the static loctr
code     loctr            followed by the code loctr
         using savf4sa,r13          map the save area
         stmg  R14,R12,SAVF4SAG64RS14 save callers registers GVBMR95
         sysstate amode64=NO
*
***NB**  entered in AMODE31, and maintains that during init phase
*        (makes it easier to use system services)
*        at label MAINLINE, changes to AMODE64
*
         llgtr R12,r15            SET   PROGRAM   BASE REGISTERS
         USING (GVBXRCK,code),R12
*
         llgtr r9,r1              LOAD  PARAMETER LIST ADDRESS
         USING GENPARM,R9

         llgt  R8,GPWORKA         LOAD  WORK  AREA ANCHOR ADDR
         ltgf  R1,0(,R8)          WORK  AREA  ALLOCATED ???
*
         BRP   CHAIN              YES - CHAIN THEM TOGETHER (RSA'S)
*
         lghi  R0,WORKLEN+8       LOAD   WORK AREA SIZE
         STORAGE OBTAIN,LENGTH=(0),LOC=(ANY),CHECKZERO=YES GET WORKAREA
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
         MVC   0(l'xrckeyeb,R1),XRCKEYEB
         aghi  r1,l'xrckeyeb      move pointer past
         drop  r13
         USING XRCKWORK,R1
         using savf4sa,xrckwork
         stg   r13,savf4saprev    save current r13
         mvc   savf4said(4),=a(savf4said_value) set 'F4SA' in area
         ST    R13,WKMR95WA       SAVE GVBMR95  WORK  AREA ADDRESS
         lgr   R13,r1             Get new workarea into r13
         ST    R13,0(,R8)         SAVE  WORK  AREA ADDRESS
*                                                                     *
*        If SNAP is required, use SNAP=Y so that MR95 allocates and   *
*        opens the file.                                              *
*        The SNAPDCB address is saved in SNAPDCBA                     *
*                                                                     *
         BRAS  R10,INITWORK       INITIALIZE    WORK  AREA
*
*        LA    R10,WORKLEN-1(,R13)
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=REGS,ID=200,STORAGE=((R13),(R10))
*        L     R10,RSABP(,R13)
*        LA    R15,72-1(,R10)
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),ID=201,STORAGE=((R10),(R15))
*
         BRU   MAINLINE           BEGIN
*
CHAIN    ds    0h
         stg   r13,savf4saprev    save current r13
         llgtr R13,r1             Get new workarea into r13
         drop  r1
         using xrckwork,r13
         using savf4sa,xrckwork
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* 1.  EMPTY LOGICAL RECORD BUFFER BUILT FOR   PREVIOUS BREAK ID       *
* 2.  FILL  LOGICAL RECORD BUFFER WITH  ENTITY RECORDS FOR NEXT ID    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
MAINLINE ds    0h
***********************************************************************
*  SWITCH TO 64-BIT MODE ("GVBXRCK" RUNS MOSTLY IN 64-BIT MODE)       *
***********************************************************************
         sysstate amode64=YES
         SAM64
         ST    R9,WKPLISTA        SAVE PARAMETER LIST ADDRESS
         DROP  R9

*
*
*        LA    R10,WORKLEN-1(,R13)
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=REGS,ID=205,STORAGE=((R13),(R10))
*
*
***********************************************************************
*  UNLOAD ANY TRANSACTIONS GENERATED FROM PREVIOUS EVENT RECORD BLOCK *
***********************************************************************
         LG    R1,WKGENBEG        LOAD CURRENT GENERATED  BDW ADDRESS
         LG    R0,WKGENEND        ANY  GENERATED TRANSACTIONS ???
         SGR   R0,R1
         CGijh R0,8,unloadg       YES - RETURN NEXT GENERATED BLOCK
*
*
***********************************************************************
*  Unload any records remaining in the LR buffer for the current      *
*  break id                                                           *
***********************************************************************
CHKUNLDB LG    R1,WKBLKBDW        LOAD CURRENT BLOCK  ADDRESS
         LG    R0,WKBLKEND        COMPUTE  REMAINING  DATA  LENGTH
         SGR   R0,R1
*
         TM    2(R1),X'10'        BLOCK ALREADY DELIVERED   ???
         BRO   UNLDDONE           YES - RESTORE "HIGHWATER" ADDR
*
         CGijh R0,8,unloadb       ALL   BLOCKS  DELIVERED   ???
*
UNLDDONE MVC   WKBLKBDW,WKBLKEND  SET   CURRENT BDW  TO  BUFFER END
*
***********************************************************************
*  CHECK FOR CONTINUATION OF INTERRUPTED LOWEST LEVEL EVENT RECORDS   *
***********************************************************************
CHKUNLDT LG    R14,WKTRNBDW       LOAD TRANSACTION BLOCK BDW ADDRESS
         LG    R15,WKTRNEND       LOAD CURRENT TRAN   ENDING ADDRESS
         SGR   R15,R14            COMPUTE TRANSACTION BLOCK  LENGTH
         cgijh R15,8,unloadt0     ANY  TRANSACTION  RECORDS  ???
*
         ltgf  R2,WKTCONT2        UNDELIVERED TRANSACTION RECORD ???
         jnp   resetbuf           No - go reset the buffers
*
         XC    WKTCONT2,WKTCONT2  RESET   CONTINUATION  INDICATOR
*
         llgt  R3,WKTCONT3        RESTORE ENTITY  OPEN LIST BREAK POINT
         llgt  R4,0(,R3)          RESTORE ENTITY  FILE AREA BREAK POINT
         llgt  R5,WKTCONT5        RESTORE CURRENT OPEN LIST ENTRY ADDR
         lgf   R6,0(,R5)          RESTORE CURRENT FILE AREA ADDR
         LG    R9,WKTCONT9        RESTORE CURRENT BUFFER  POINTER
         llgt  R10,WKTCONTA       RESTORE CURRENT RETURN  ADDRESS
*
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=REGS,ID=206
*
         LG    R0,WKTRNBDW        RESET CURRENT TRANSACTION    POINTER
         AGHI  R0,4
         STG   R0,WKTRNEND
*
         BRU   OUTPUTT            ("WKOUTLEN" ALREADY SET, DECOMP DONE)
*
*
***********************************************************************
*  RESET "LR BUFFER" POINTERS TO REFILL WITH NEXT BREAK ID            *
*  INITIALIZE FIRST "BDW" FOR FIRST BLOCK IN THE "LR BUFFER" (SET R9) *
***********************************************************************
RESETBUF LG    R9,WKBUFBEG        LOAD   BUFFER  BEGINNING ADDRESS
         XGR   R0,R0              ZERO   BDW     LENGTH
         STG   R0,0(,R9)          SET    BDW     FLAGS
         STG   R9,WKBLKBDW        RESET  CURRENT POINTER   TO  BDW
         AGHI  R9,4               SKIP   BDW  TO CURRENT   RDW
         STG   R9,WKBLKEND        RESET  CURRENT POINTER   TO  EOD
         STG   R9,WKBUFEND        RESET  CURRENT POINTER   TO  EOD
*
         LG    R14,WKTRNBDW       LOAD   BUFFER  END       ADDRESS
         STG   R14,WKBLKMAX
         LG    R14,WKBUFHI        RESET  CURRENT HIGHWARER MARK
         STG   R14,WKBLKHI
*
***********************************************************************
*  INITIALIZE  SHARED  DIRECTORY                                      *
***********************************************************************
RESETDIR llgt  R14,WKDIRBEG       LOAD SHARED DIRECTORY ADDRESS
         MVC   0(4,R14),WKLSTBEG
         llgt  R15,WKDIROFF       LOAD SHARED DIRECTORY LENGTH rtc19544
         aghi  R15,DIRLEN-4       INCL GENERATED DIR (ADJ FOR 1ST & EX)
         LA    R1,4(,R14)
         if    cij,r15,ge,1       make sure length is ok
*
           LA  R15,255(,R15)
           SRLG R0,R15,8
           BRU RSET_MVC1
RSET_MVC0  MVC 0(256,R1),0(R14)   PROPAGATE 256 BYTES
           LA  R14,256(,R14)      ADVANCE   TARGET
           LA  R1,256(,R1)        ADVANCE   SOURCE
RSET_MVC1  BRCT R0,RSET_MVC0
           exrl R15,MVCR1R14     PROPAGATE REMAINDER
         endif
*
*
***********************************************************************
*  INITIALIZE  BEGINNING/CURRENT TABLE POINTERS                       *
***********************************************************************
         llgt  R14,WKLSTBEG
         ST    R14,WKLSTCUR
*
         llgt  R14,WKPROBEG
         ST    R14,WKPROEND
         MVC   0(PROTBLEN,R14),XHEXFF
*
*
***********************************************************************
*  INITIALIZE TRANSACTION POINTERS (GENERATED POINTERS ARE TEMPORARY) *
***********************************************************************
         LG    R0,WKbufbeg        LOAD  TRANSACTION BUFFER ADDRESS
         STG   R0,WKGENBEG        RESET CURRENT  GENERATED BUFR POINTER
         STG   R0,WKGENBDW        RESET CURRENT  GENERATED BDW  POINTER
         STG   R0,WKGENEND        RESET CURRENT  GENERATED TRAN POINTER
*
         LG    R0,WKTRNBDW        LOAD  TRANSACTION BUFFER ADDRESS
         AGHI  R0,4               RESET CURRENT  TRANSACTION    POINTER
         STG   R0,WKTRNEND
*
         XC    WKLSTREC,WKLSTREC  RESET LAST  RECORD POINTER
*
*
***********************************************************************
*  LOCATE NEXT LOWEST VALUE FOR BREAK ID AMONGST ALL FILES            *
***********************************************************************
         XC    WKMAXKEY,WKMAXKEY  ZERO MAXIMUM  KEY   LEN
*
         llgt  R1,WKBRKLOW        RE-INITIALIZE LOWEST BREAK KEY
         LA    R14,1(,R1)
         MVI   0(R1),X'FF'
         llgt  R15,WKSAVLEN
         if    cij,r15,gt,1       make sure length is ok
*
           LA  R15,255-1(,R15)
           SRLG R0,R15,8
           BRU RSETMVC1
RSETMVC0   MVC 0(256,R14),0(R1)   PROPAGATE 256 BYTES
           LA  R14,256(,R14)      ADVANCE   TARGET
           LA  R1,256(,R1)        ADVANCE   SOURCE
RSETMVC1   BRCT R0,RSETMVC0
           exrl R15,MVCR14R1      PROPAGATE REMAINDER
         endif
*
         SGR   R3,R3              ZERO LOWEST KEY FILE AREA  ADDRESS
*
         llgt  R5,WKOPNBEG        LOAD FIRST OPEN FILE LIST  ENTRY ADDR
*
NEXTBRKI ltgf  R6,0(,R5)          LOAD  FILE   CONTROL BLOCK ADDRESS
         USING FILEAREA,R6
         BRNP  NEXTDONE           NO  - SEARCH DONE
*
         CLI   FILEETYP,C'V'      VIRTUAL ENTITY ???
         BRE   NEXTFILE
*
         LG    R7,FILERECA        LOAD  RECORD  ADDRESS
*
NEXTRSRC CLI   FILERSRC,C'S'      SUBSTITUTION  FILE   ???
         BRNE  nextrsrc5
         AGHI  R7,1               YES - ADVANCE OVER   ACTION CODE
*
NEXTRSRC5 ds   0h
         LGH   R2,FILEBRKL        LOAD  BREAK ID LENGTH
*
         if CLI,WKEXTFLG,eq,C'Y'      EXTRACT RECORD FORMAT ???
*
           Lgh R2,WKMAXKEY        LOAD  SORT KEY LENGTH
           if LTgr,R2,R2,np
             LGH R2,0(,R7)
             if CLC,FILERTYP,ne,NETCHG
               AGHI R2,-2
             endif
             STH R2,WKMAXKEY
           endif
         endif
         LTGR  R3,R3              ANY   RECORD SELECTED  ???
         BRNP  NEXTSAVE           NO  - SAVE  1ST ONE
*
         LGH   R1,FILEKEYO        LOAD  BREAK  ID OFFSET
         AGR   R1,R7
         llgt  R14,WKBRKLOW
         LA    R15,255(,R2)
         SRLG  R0,R15,8
         BRU   NEXTCLC1
NEXTCLC0 CLC   0(256,R1),0(R14)   COMPARE  256 BYTES
         BRNL  NEXTFILE
         LA    R1,256(,R1)        ADVANCE  TARGET
         LA    R14,256(,R14)      ADVANCE  SOURCE
NEXTCLC1 BRCT  R0,NEXTCLC0
         exrl  R15,CLCR1R14       COPY  REMAINDER
         BRNL  NEXTFILE
*
*
NEXTSAVE LGH   R1,FILEKEYO        LOAD  BREAK  ID OFFSET
         AGR   R1,R7
         llgt  R14,WKBRKLOW
*
         LA    R15,255(,R2)
         SRLG  R0,R15,8
         BRU   NEXTMVC1
NEXTMVC0 MVC   0(256,R14),0(R1)   MOVE 256 BYTES
         LA    R14,256(,R14)      ADVANCE  TARGET
         LA    R1,256(,R1)        ADVANCE  SOURCE
NEXTMVC1 BRCT  R0,NEXTMVC0
         exrl  R15,MVCR14R1       COPY  REMAINDER
*
         ST    R2,WKSAVLEN        SAVE LOWEST BREAK KEY LEN
         LGR   R3,R5              SAVE ASSOCIATED OPEN LIST  ENTRY
*
NEXTFILE AGHI  R5,4
         BRU   NEXTBRKI
*
*
***********************************************************************
*  SEARCH FOR NEXT LOWEST BREAK ID IS DONE,  WERE ANY RECORDS FOUND ? *
*  CHECK  FOR RECORDS OUT OF SEQUENCE ERROR  CONDITION                *
***********************************************************************
NEXTDONE LTGR  R3,R3              ANY  RECORD   FOUND  ???
         BRNP  DONE               NO  -  SEND   LAST   LR BUFFER
*
         Llgt  R1,WKTOKNPA
         USING PARTAREA,R1
         agsi  PARTBRKC,b001      INCREMENT BREAK ID  COUNT
         DROP  R1
*
         llgt  R0,WKBRKOLD        SAVE   PREVIOUS BREAK  ID
         MVC   WKBRKOLD,WKBRKID   UPDATE PREVIOUS BREAK  ID
         MVC   WKBRKID,WKBRKLOW   UPDATE CURRENT  BREAK  ID
         ST    R0,WKBRKLOW        UPDATE NEXT     LOWEST
*
*        L     R10,WKBRKID
*        LR    R15,R10
*        LA    R15,25-1(,R15)
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=REGS,ID=207,STORAGE=((R10),(R15))
*
         ST    R3,WKOPNCUR        SAVE STARTING OPEN FILE LIST ENTRY
         LGR   R5,R3              LOAD BREAK    OPEN FILE LIST ENTRY
         lgf   R6,0(,R5)          LOAD  FILE CONTROL AREA ADDRESS
*
         CLI   WKEXTFLG,C'Y'      EXTRACT RECORD FORMAT ???
         BRNE  NEXTSEQ
*
         LG    R7,FILERECA        LOAD  RECORD  ADDRESS - LOWEST
         LH    R15,WKMAXKEY       LOAD  MAX  KEY LENGTH
         STH   R15,WKBRKLEN
*
NEXTSEQ  llgt  R1,WKBRKID         FILE "OUT-OF-SEQUENCE"
         llgt  R14,WKBRKOLD
*
         LGHI  R15,L'WKKEYSV1+255
         SRLG  R0,R15,8
         BRU   NEXTCLC5
NEXTCLC4 CLC   0(256,R1),0(R14)   COMPARE  256 BYTES
         BRNL  MERGENTY
         LA    R1,256(,R1)        ADVANCE  TARGET
         LA    R14,256(,R14)      ADVANCE  SOURCE
NEXTCLC5 BRCT  R0,NEXTCLC4
         exrl  R15,CLCR1R14       COPY  REMAINDER
         BRNL  MERGENTY
*
         llgt  R1,WKBRKID         FILE "OUT-OF-SEQUENCE"
         llgt  R14,WKBRKOLD
         LGH   R15,WKBRKLEN
*
         LA    R15,255(,R15)
         SRLG  R0,R15,8
         BRU   NEXTCLC9
NEXTCLC8 CLC   0(256,R1),0(R14)   COMPARE  256 BYTES
         BRNL  MERGENTY
         LA    R1,256(,R1)        ADVANCE  TARGET
         LA    R14,256(,R14)      ADVANCE  SOURCE
NEXTCLC9 BRCT  R0,NEXTCLC8
         exrl  R15,CLCR1R14       COPY  REMAINDER
         BRE   MERGENTY
*
NEXTSORT DS    0H
         l r14,fileprma           address parameter area
         using parmtbl,r14
         if cli,parmdups,e,C'Y'
           J    MERGENTY
         endif
         drop r14
*
         lgf   R6,0(,R5)         LOAD FILE AREA ADDRESS FROM OPEN LIST
         Llgt  R2,WKBRKID
         GVBMSG FORMAT,MSGNO=CK_OUT_OF_SEQUENCE,SUBNO=3,               +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(UR20DDN,L'UR20DDN),                               +
               SUB3=(0(,R2),24),             Gen record/key            +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*  FILL "LR BUFFER" WITH RECORDS FOR THE CURRENT BREAK ID             *
*  MERGE RECORDS FOR THE SAME ID BY ENTITY IN KEY SEQUENCE            *
*                                                                     *
*  FOR EACH ENTITY OUTPUT LOWEST VALUED KEY FOR THE CURRENT BREAK     *
*  REPEAT FOR EACH ENTITY UNTIL NO MORE RECORDS WITH THAT ID          *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
MERGENTY ST    R3,WKOPNBRK        INITIALIZE    BREAK  POINT
*
         lgf   R6,0(,R5)          INITIALIZE FILE  AREA ADDR "T" OPTION
         USING FILEAREA,R6
*
         LTGR  R4,R6              LOAD FIRST OPEN  FILE AREA BREAK ADDR
         BRNP  MERGDONE
*
         XC    WKRECPRV,WKRECPRV  INITIALIZE PREVIOUS  RECORD ADDR
*
         LG    R2,FILERECA        INITIALIZE RECORD  ADDRESS "T" OPTION
*
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=REGS,ID=206
*
         ltgf  R14,FILEMINA-FILEAREA(,R6)
         BRNP  MERGKEY
*
         MVC   0(4,R14),WKLSTCUR  INITIALIZE SHARED DIRECTORY - MIN
         MVC   4(4,R14),WKLSTCUR                              - MAX
*
***********************************************************************
*  CALL "READ EXIT" FOR VIRTUAL ENTITIES                              *
***********************************************************************
         CLI   FILEETYP,C'V'      VIRTUAL  ENTITY  ???
         BRNE  MERGKEY
*
         if ltgf,R0,FILERXEP,nz   READ  EXIT SPECIFIED  ???
*
           XC  WKRXITRC,WKRXITRC  ZERO  PREVIOUS RETURN CODE
*
           llgt R14,FILEPRMA      RELATED PARMTBL ENTRY
           LA  R1,PARMRXPM-PARMTBL(,R14)
           ST  R1,WKCALLP3
*
           XC  WKCALL64,WKCALL64  Null 64-bit Event Record address
           LAY R1,WKCALL64        Load pointer to the address
           STY R1,WKCALLP4        Store pointer in parm list
*
           LA  R1,FILERXAN        WORK    AREA   ANCHOR
           ST  R1,WKCALLP7
           LA  R1,WKRXITRC        RETURN  CODE
           ST  R1,WKCALLP8
*
MERGVIRT   do until=(cij,r15,ne,4) CALL  READ     EXIT
             LA R1,WKCALLPL       CALL  READ     EXIT
             llgf R15,FILERXCA    (R0: TRUE ENTRY POINT)
             BASsm R14,R15
*
             LGF R15,WKRXITRC     CHECK RETURN   CODE
             if Cij,R15,eq,16     abort?
               B RTNABEND
             endif
             doexit cij,r15,ge,8
*
             lg R2,WKCALLPT       COPY  RETURNED RECORD TO "LR BUFFER"
             STG R2,FILERECA
             LGF R0,WKCALLRL      RECORD  LENGTH
             STH R0,FILERECL
             STH R0,FILEMAXL
             agsi FILEICNT,B001
             BRAS R10,OUTPUT
*
             LGF R15,WKRXITRC     CHECK  RETURN   CODE FOR "RECALL"
           enddo
*
         endif
MERGVNXT AGHI  R5,4               ADVANCE TO NEXT FILE
         lgf   R6,0(,R5)
*
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=REGS,ID=208
         BRU   MERGBRK
*
                        EJECT
MERGKEY  SGR   R2,R2              INDICATE   RECORD NOT SELECTED
         ST    R2,WKSUBAVA        INDICATE   NO SUBSTITUTION RECORD YET
*
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=REGS,ID=209
*
MERGLOOP ltgf  R6,0(,R5)          LOAD  FILE CONTROL AREA   ADDRESS
         BRNP  MERGRFND           NO - CHECK IF  ANY RECORDS FOUND
*
         CLC   FILERTYP,FILERTYP-FILEAREA(R4)  BREAK IN  REC TYPE ???
         BRNE  MERGRFND                        YES - CHECK IF REC FOUND
*
*
***********************************************************************
*  LOCATE LOWEST VALUED KEY FOR THIS ENTITY(OUTPUT RECORDS IN KEY SEQ)*
***********************************************************************
MERGREC  LG    R7,FILERECA        LOAD  RECORD  ADDRESS - CURRENT
*
MERGRSRC CLI   FILERSRC,C'S'      SUBSTITUTION  FILE ???
         BRNE  mergrsrc_5
         AGHI  R7,1               YES - ADVANCE OVER ACTION  CODE
*
*        LH    R10,FILERECL
*        AR    R10,R7
*        BCTR  R10,0
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=REGS,ID=208,STORAGE=((R7),(R10))
*
MERGRSRC_5 ds  0h
         LGH   R14,FILEKEYO       LOAD  BREAK   ID   OFFSET
         LTGR  R14,R14            SAVE  KEY OFFSET
         BRNM  MERGCHK
*
         SGR   R14,R14
         BRU   MERGALL
*
MERGCHK  AGR   R14,R7
         llgt  R1,WKBRKID
*
         LGH   R15,FILEBRKL
         if CLI,WKEXTFLG,eq,c'Y'      EXTRACT RECORD FORMAT ???
           LGH R15,WKBRKLEN       LOAD  SORT KEY LENGTH
         endif
*
         LA    R15,255(,R15)
         SRLG  R0,R15,8
         BRU   MERGCLC1
MERGCLC0 CLC   0(256,R1),0(R14)   COMPARE  256 BYTES
         BRE   MERGALL            YES - PROCESS
         BRL   MERGNEXT           NO  - SKIP TO NEXT FILE   RECORD
         LA    R1,256(,R1)        ADVANCE  TARGET
         LA    R14,256(,R14)      ADVANCE  SOURCE
MERGCLC1 BRCT  R0,MERGCLC0
         exrl  R15,CLCR1R14       COPY  REMAINDER
         BRE   MERGALL            YES - PROCESS
         BRL   MERGNEXT           NO  - SKIP TO NEXT FILE   RECORD
*
         CLI   FILEETYP,C'T'      TRANSACTION ENTITY ???
         BRNE  MERGDUPS
*
MERGALL  LTGR  R2,R2              FIRST KEY   FOUND  ???
         JZ    MERGSAVE           YES - SAVE  FOUND  REGISTERS
*
MERGLOWR LGH   R1,FILEKEYO        LOAD  KEY   OFFSET
         LGR   R14,R2
         AGR   R14,R1
         AGR   R1,R7
         LGH   R15,FILEKEYL       LOAD  KEY   LENGTH
*
         LA    R15,255(,R15)
         SRLG  R0,R15,8
         BRU   MERGCLC5
MERGCLC4 CLC   0(256,R1),0(R14)   COMPARE  256 BYTES
         BRH   MERGNEXT           NO  - CHECK NEXT FILE
         BRL   MERGSAVE           NO  - CHECK NEXT FILE
         LA    R1,256(,R1)        ADVANCE  TARGET
         LA    R14,256(,R14)      ADVANCE  SOURCE
MERGCLC5 BRCT  R0,MERGCLC4
         exrl  R15,CLCR1R14       COPY  REMAINDER
         BRH   MERGNEXT           NO  - CHECK NEXT FILE
         BRL   MERGSAVE           NO  - CHECK NEXT FILE
*
***********************************************************************
*  WHEN KEYS ARE EQUAL USE EFFECTIVE DATE AS "TIE BREAKER"            *
***********************************************************************
         LGH   R1,FILEEFDO        EFFECTIVE  DATES PRESENT ???
         LTGR  R1,R1
         BRM   MERGDUPS           NO - CHECK IF DUPLICATES ALLOWED ??
*
         LGR   R14,R2             LOAD PREVIOUS LOWEST RECORD ADDRESS
         AGR   R14,R1
         AGR   R1,R7
         CLC   0(4,R1),0(R14)     LOWER EFFECTIVE  DATE  ???
         JH    MERGNEXT           High - skip
         JL    MERGSAVE           Low  - save
*
***********************************************************************
*  KEYS ARE EQUAL, CHECK IF DUPLICATES ARE ALLOWED                    *
***********************************************************************
MERGDUPS CLI   FILECLAP,C' '      COLLAPSE OPTION SPECIFIED ???
         BRNE  MERGNEXT           YES - KEEP 1ST  VALUE  RECORD
*
         llgt  r14,fileprma       address parmeter area
         using parmtbl,r14
         if cli,parmdups,e,C'Y'
           J    MERGNEXT
         endif
         drop r14
*
MERGSORT DS    0H                 DISPLAY "SORT SEQUENCE ERROR" MSG
         lgf   R6,0(,R5)          LOAD FILE AREA ADDRESS FROM OPEN LIST
         LGH   R2,FILEKEYO        LOAD KEY  OFFSET
         AGR   R2,R7
*
         GVBMSG FORMAT,MSGNO=CK_OUT_OF_SEQUENCE,SUBNO=3,               +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(UR20DDN,L'UR20DDN),                               +
               SUB3=(0(,R2),24),                Record key             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
         BRU   RTNERROR
*
MERGSAVE ST  R5,WKFILELO          SAVE  LOWEST KEY VALUE FILE AREA ADR
         LGR R2,R7                SAVE  LOWEST KEY VALUE REC  ADDRESS
*
MERGNEXT AGHI  R5,4               ADVANCE  TO NEXT FILE
         BRU   MERGLOOP
                        EJECT
***********************************************************************
*  CHECK IF LOWEST KEY VALUED RECORD FOUND  FOR THE CURRENT BREAK ID  *
*  CHECK IF IT  IS A SUBSTITUTION    RECORD                           *
*  IF SUBSTITUTION RECORD THEN PROCESS BASED ON ACTION CODE           *
***********************************************************************
MERGRFND LTGR  R7,R2              ANY  BREAK RECORDS FOUND ???
         BRNP  MERGBRK            NO - NOTHING FROM THIS FILE
*
         llgt  R5,WKFILELO        POINT TO LOWEST VALUED KEY  FILE AREA
         lgf   R6,0(,R5)
*
         lgf   R15,FILERXRC       Load RC from previous call      @008I
*                                                                 @008I
MERGCALL DS    0H                 Call "READ EXIT" if specified   @008C
         LT    R0,FILERXEP                                        @008C
         JZ    MERGKXIT                                           @008I
*                                                                 @008I
         LH    R1,FILERECL        Initialze record lengthH        @008I
         ST    R1,WKCALLRL                                        @008I
         ST    R1,FILERXRL                                        @008I
         J     MERGRXIT                                           @008I
*                                                                 @008I
MERGKXIT do ,                     Check if "KEY EXIT" specified   @008I

           if LT,R0,FILEKXEP,nz                                   @008I
*                                                                 @008I
             MVC WKCALL64,FILERXPT Refresh event record address   @008I
             MVC WKCALLPT,FILERXPT Refresh result record address  @008I
             MVC WKCALLRL,FILERXRL Refresh result record length   @008I
*                                                                 @008I
             ltgr R15,R15         Check RC from previous call     @008I
             JZ MERGRXRA                                          @008I
*                                                                 @008I
MERGRXIT     DS 0H                                                @008I
             Llgt R14,FILEPRMA   RELATED  PARMTBL ENTRY
             LA R1,PARMRXPM-PARMTBL(,R14)
             ST R1,WKCALLP3
*
             STG R2,WKCALL64      Initialize Event Record address
             LAY R1,WKCALL64      Load pointer to the address
             STY R1,WKCALLP4      Store pointer in parm list
*
             stg R2,WKCALLPT      Initialize return record address@008I
*                                                                 @008I
             MVC WKCALLRL,FILERXRL Initialize event record length @008I
*                                                                 @008I
             LA R1,FILERXAN       WORK AREA ANCHOR
             ST R1,WKCALLP7
*                                                                 @008I
             LA R1,FILERXRC       RETURN  CODE                    @008C
             ST R1,WKCALLP8
*
             XC FILERXRC,FILERXRC ZERO RETURN CODE                @008C
*
             LA R1,WKCALLPL
             llgf R15,FILERXCA    (R0: TRUE ENTRY POINT)
             BASsm R14,R15
*
             LGF R15,FILERXRC     PROCESS RECORD ??
             if Cij,R15,eq,16     ABORT   ???
               J RTNabend                                         @008C
             endif
             if Cij,R15,ge,8            RC= 0 or 4??
               doexit cij,r15,eq,12 .Don't write any records?
*
               agsi FILEDCNT,b001 INCREMENT  DUPLICATE  COUNT
               leave ,
             endif
*
MERGRXRA     DS 0H                                                @008I
             lg R2,WKCALLPT       RETURNED   RECORD ADDRESS
             LGF R0,WKCALLRL      RETURNED   RECORD LENGTH
             STH R0,WKOUTLEN      (LEAVE "FILERECL" INTACT)
*
             BRAS R10,OUTPUT0     ("WKOUTLEN" ALREADY  SET)
*
             LGF R15,FILERXRC     CHECK  RETURN   CODE FOR "RECALL"
             doexit cij,r15,ne,4
*
             agsi FILEICNT,b001   INCREMENT  INPUT REC  COUNT
*
             LG R2,WKCALL64       Re-Initialize Event Record address
             J MERGRXIT                                           @008C
*
           else
             BRAS R10,OUTPUT      PASS LOWEST KEYED RECORD
           endif
         enddo ,
         larl  r15,readrec        get the address
         basr  R10,r15            and go there flipping modes as we go
*
         llgt  R5,WKOPNBRK        RESTART AT TOP OF ENTITY SET
         BRU   MERGKEY
*
***********************************************************************
*  BREAK IN CURRENT ENTITY, ADVANCE TO NEXT ENTITY (IF ANY)           *
***********************************************************************
MERGBRK  ltgf  R14,FILEMINA-FILEAREA(R4)  SHARED DIRECTORY - MAX
         BRNP  mergbrk_5
         MVC   4(4,R14),WKLSTCUR
*
MERGBRK_5 ds   0h
         LTG   R14,WKRECPRV       MARK  THE LAST RECORD OF THE SET
         BRNP  MERGLAST
         OI    2(R14),X'40'
*
         CLI   FILEKMAP-FILEAREA(R4),X'00'  MARK LAST  KEY VALUE REC ??
         BRE   MERGLAST           NO - LEAVE AS IS
         MVC   4+L'FILEPRFX-1(1,R14),FILEKMAP-FILEAREA(R4)        @018C
*
MERGLAST LTGR  R6,R6              END-OF-OPEN-FILE LIST     ???
         BRNP  MERGDONE           YES - PASS BUFFER  TO    "GENEVA"
*
         LGR   R3,R5              SAVE BREAK OPEN  LIST    ENTRY ADDR
         BRU   MERGENTY           LOOP UNTIL NEXT  RECORD  TYPE  BREAK
*
         ds    0d
MSGCOPY  MVC   WKTXTLEN(0),0(R14) * * * * *  E X E C U T E D  * * * * *
         ds    0d
GENCOPY  MVC   WKTXTLEN-XRCKWORK(0,R8),0(R14)  X E C U T E D  * * * * *
*
*
***********************************************************************
*   MERGING FOR THE CURRENT BREAK ID IS DONE                          *
*   COMPLETE "BDW" FOR LAST BLOCK IN "LR BUFFER"                      *
***********************************************************************
MERGDONE LTG   R1,WKLSTREC        MARK  THE LAST   RECORD OF THE SET
         BRNP  MERGBLK1
         OI    2(R1),X'80'
         XC    WKLSTREC,WKLSTREC
*
MERGBLK1 LG    R14,WKBLKBDW       LOAD CURRENT BDW  ADDRESS
         LG    R15,WKBLKEND       LOAD CURRENT DATA  ENDING ADDRESS
         LGR   R0,R15             COMPUTE LAST BLOCK LENGTH
         SGR   R0,R14
         STH   R0,0(,R14)         BUILD   BDW
*
***********************************************************************
*   UPDATE END-OF-DATA ADDRESS FOR GENERATED TRANS AREA TO BE FILLED  *
***********************************************************************
         STG   R15,WKGENBEG       GENERATED  TRANSACTIONS FOLLOW BUFFER
         STG   R15,WKGENBDW
         lgr   r1,r15
         aghi  R1,4
         STG   R1,WKGENEND
*
         XC    0(8,R15),0(R15)    ZERO  BDW/RDW
*
*
         STG R15,WKBUFEND     SET   PRIMARY DATA END
         MVC WKBUFHI,WKBLKHI
         MVC WKGENMAX,WKTRNBDW STILL IN PRIMARY LR BUFFER
*
***********************************************************************
*   BEGIN UNLOADING BLOCKS IN "LR BUFFER"                             *
***********************************************************************
         LG    R1,WKBUFEND        LOAD  LR BUFFER END-OF-DATA   ADDRESS
         STG   R1,WKBLKEND        SAVE  CURRENT   AREA   END-OF-DATA
         LG    R1,WKBUFBEG        LOAD  BEGINNING BLOCK  ADDRESS
         STG   R1,WKBLKBDW        START UNLOADING FROM   BEGINNING
*
         LH    R0,0(,R1)          ANY   RECORDS IN BUFFER ???
         if Cij,R0,gt,8,and,                                           +
               TM,2(R1),X'10',no  Not DELIVERED  ???
*
           LT  R0,WKBXITEP        CALL "BUFFER READY" EXIT IF SPECIFIED
           BRZ UNLOADB
*
           XC  WKEXITRC,WKEXITRC  ZERO  RETURN  CODE
           LA  R1,WKBXITPM
           ST  R1,WKCALLP3
           XC  WKCALL64,WKCALL64  Null 64-bit Event Record address
           LAY R1,WKCALL64        Load pointer to the address
           STY R1,WKCALLP4        Store pointer in parm list
           LA  R1,WKBXITAN        WORK  AREA  ANCHOR
           ST  R1,WKCALLP7
           LA  R1,WKEXITRC        RETURN  CODE
           ST  R1,WKCALLP8
*
           LA  R1,WKCALLPL
           llgf R15,WKBXITCA
           bassm R14,R15
           lgf R15,WKEXITRC       CHECK RETURN   CODE
           Cije R15,16,rtnabend   ABORT ???
*
           LG  R1,WKBLKBDW        LOAD  FIRST BLOCK   ADDRESS
           BRU UNLOADB            UNLOAD "LR BUFFER"
*
         endif
*
***********************************************************************
*   RETURN TRANSACTION BLOCK IF NOT EMPTY                             *
***********************************************************************
UNLOADT  LG    R14,WKTRNBDW       LOAD TRANSACTION BLOCK BDW ADDRESS
         LG    R0,WKTRNEND
         SGR   R0,R14
         Cgijnh R0,8,resetbuf
*
UNLOADT0 llgt  R9,WKPLISTA        LOAD PARAMETER   LIST  ADDRESS
         USING GENPARM,R9
*
         Llgt  R1,GPBLOCKA        LOAD POINTER ADDRESS
         stg   R14,0(,R1)         RETURN BLOCK ADDRESS
*
         Lgh   R15,0(,R14)        RETURN BLOCK LENGTH
         Llgt  R1,GPBLKSIZ
         ST    R15,0(,R1)
*
         AGHI  R14,4              RESET ENDING ADDRESS (AFTER "BDW")
         STG   R14,WKTRNEND
*
         cijnh r15,8,resetbuf     ANY  RECORDS IN TRANACTION BUFFER ???+
                                  NO  -NOTHING TO DELIVER  FOR THIS KEY
*
*        LLGT  R14,GPBLOCKA       LOAD POINTER ADDRESS
*        L     R2,0(,R14)
*        LH    R3,0(,R2)
*        LA    R10,0(R2,R3)
*        BCTR  R10,0
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),ID=208,STORAGE=((R2),(R10))
*
         BRU   RETURN0
*
*
***********************************************************************
*   RETURN NEXT GENERATED RECORD BLOCK                                *
***********************************************************************
UNLOADG  llgt  R9,WKPLISTA
         USING GENPARM,R9
*
         LGH   R15,0(,R1)         RETURN BLOCK  LENGTH
         Llgt  R14,GPBLKSIZ
         ST    R15,0(,R14)
*
         Llgt  R14,GPBLOCKA       LOAD  POINTER ADDRESS
         stg   R1,0(,R14)         RETURN BLOCK ADDRESS
*
         LGR   R2,R1              copy ORIGINAL ADDRESS
         AGR   R2,R15             ADVANCE TO NEXT BLOCK (IF ANY)
         STG   R2,WKGENBEG        Note R2 used below after endif
*
         lg    r1,wkgenend        get end of block
         cgr   r2,r1              LAST BLOCK IN  AREA ???
         BRL   RETURN0            NO  - SKIP STARTING OF NEW BLOCK

***********************************************************************
*   LAST BLOCK IN AREA BEING DELIVERED - Update high water mark etc   *
***********************************************************************
*
*
         LG    R0,WKBLKHI
         if CG,R0,gt,WKBUFHI
           STG R0,WKBUFHI
         endif
*
         LGR   R0,R1              ENOUGH  ROOM  FOR  ANOTHER BLOCK ???
         AGHI  R0,8
         CG    R0,WKGENMAX
         BRNL  RETURN0            NO  -   SKIP  STARTING NEW BLOCK
*
         STG   R1,WKGENBDW        START A NEW BLOCK (IN CASE MORE GEN)
         xr    R15,R15            ZERO  BDW
         ST    R15,0(,R1)
         AGHI  R1,4
         STG   R1,WKGENEND
*
         BRU   RETURN0
*
***********************************************************************
*   RETURN NEXT "BUFSIZE" buffer                                      *
***********************************************************************
UNLOADB  llgt  R9,WKPLISTA        LOAD PARAMETER  LIST ADDRESS
         USING GENPARM,R9
*
         LGH   R15,0(,R1)         RETURN BLOCK LENGTH
         Llgt  R14,GPBLKSIZ
         ST    R15,0(,R14)
*
         Llgt  R14,GPBLOCKA       LOAD  POINTER ADDRESS
         stg   R1,0(,R14)         RETURN BLOCK ADDRESS
*
         OI    2(R1),X'10'        MARK BLOCK AS DELIVERED
*
         LGR   R2,R1              Copy ORIGINAL ADDRESS
         AGR   R2,R15
         STG   R2,WKBLKBDW        ADVANCE TO NEXT BLOCK
*
RETURN0  EQU   *
*
         sgr   R15,R15            SET  RETURN CODE  TO ZERO
         Llgt  R14,GPRTNCA        INDICATE NORMAL COMPLETION
         ST    R15,0(,R14)
*
return   ds 0h
         lg    r13,savf4saprev         restore caller save area
         lg    r14,SAVF4SAG64RS14      restore caller's r14
         lmg   r2,r12,SAVF4SAG64RS2    restore caller's r2- r12
         BSM   0,R14              RETURN
*
RTNERROR ds    0h
*
         llgt  R9,WKPLISTA
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
         llgt  R14,GP_ERROR_BUFFER_PTR ERROR MESSAGE BUFFER ADDRESS
         LA    R1,WKTXTBUF+4
         exrl  R15,MVCR14R1
*
         lghi  R15,16             SET RETURN CODE  REGISTER TO 16
         ST    R15,GP_ERROR_REASON
*
RTNABEND ds    0h
*
         llgt  R9,WKPLISTA
         llgt  R14,GPRTNCA        INDICATE NORMAL COMPLETION
         ST    R15,0(,R14)
         BRU   RETURN
         DROP  R3 GENENV
*
                        EJECT
***********************************************************************
*   PRINT CONTROL REPORT  AND  CLOSE FILES                            *
*   PASS  "END-OF-FILE"   TO  "GENEVA"                                *
***********************************************************************
DONE     Llgt  R9,WKPLISTA        LOAD PARAMETER LIST ADDRESS
         USING GENPARM,R9
*
         llgt  R1,WKMR95WA
         USING THRDAREA,R1
*
         Llgt  R14,GPENVA
         CLC   GPPHASE-GENENV(L'GPPHASE,R14),OP
         BRE   DONECLOS
*
***********************************************************************
*  SWITCH TO "TCB" MODE IF ZIIP ENABLED                               *
*      (R14: "TCB" MODE ADDRESS)                                      *
***********************************************************************
           if (ltgf,r15,workazip,nz),and,  zIIP function available?    +
               (cli,thread_mode,eq,C'S')   and we are in SRB mode
             lgr  r8,r13              save address of XRCK WA
             lgr  r13,r1              use thread work area
             la     r1,TCB_switch     Switch to TCB mode
             bassm r14,r15            Call zIIP module
             lgr  r13,r8              restore XRCK WA
           endif
*
DONECLOS larl  R15,Closfile       CLOSE    ENTITY FILES
         oilh  r15,X'8000'        make it amode 31
         bassm r14,r15
*
         LHI   R15,8
         Llgt  R14,GPRTNCA        INDICATE NORMAL COMPLETION
         ST    R15,0(,R14)
         BRU   RETURN
*
         DROP  R6
         DROP  R9
                        EJECT
         ds    0d
MVCR1R14 MVC   0(0,R1),0(R14)     * * * * E X E C U T E D * * * *
         ds    0d
MVCR14R1 MVC   0(0,R14),0(R1)     * * * * E X E C U T E D * * * *
         ds    0d
CLCR1R14 CLC   0(0,R1),0(R14)     * * * * E X E C U T E D * * * *
static   loctr
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O N S T A N T S                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         DS   0D
F310     DC    F'100'
F6000    DC    F'6000'
F360K    DC    F'360000'
F32K     equ     32768
F10MEG   DC    F'10240000'
F25MEG   equ     25600000
TOKNPERS DC    F'0'                 TOKEN PERSISTENCE
DYNWAIT  DC    F'4000'
RESMWAIT DC    F'0010'
TSTMPLEN DC    A(L'WKTSTMP)
*
MODE31   equ   X'8000'
         DS   0D
OPENPARM DC    XL8'8000000000000000'
*
H000     DC    H'000'
H004     DC    H'004'
H008     DC    H'008'
H012     DC    H'012'
BLKSIZE  equ     32760
*
P000     DC    PL1'0'
P001     DC    PL1'1'
B001     equ   1
P99      DC    PL2'99'
ZACCUM   DC    PL(L'COLDATA)'0'
*
BLANKS   DC    CL(parmlen)' '
*
XRCKEYEB DC    CL8'XRCKWORK'
XRCKFILE DC    CL8'XRCKFILE'
XRCKBUFR DC    CL8'XRCKBUFR'
XRCKtran DC    CL8'XRCKTRAN'
CKTKEYEB DC    CL8'CKTKNWRK'
CKTREYEB DC    CL8'CKTRNWRK'
CKVREYEB DC    CL8'CKVRTWRK'
VIEWEYEB DC    CL8'XRCKVIEW'
GLOBEYEB DC    CL8'XRCKGLOB'
PARTEYEB DC    CL8'XRCKPART'
DCMPEYEB DC    CL8'XRCKDCMP'
NETCHG   DC    CL8'NETCHG  '
DB2      DC    CL3'DB2'
LECOBOL  DC    CL3'CEE'           COBOL  VERSION IDENTIFIER
*
OP       DC    CL2'OP'
CL       DC    CL2'CL'
*
SHR      DC    CL3'SHR'
DSN      DC    CL3'DSN'
DCMP     DC    CL4'DCMP'
NEWB     DC    CL4'NEWB'
NBLK     DC    CL4'NBLK'
KXIT     DC    CL4'KXIT'                                          @008I
RXIT     DC    CL4'RXIT'
PXIT     DC    CL4'PXIT'
OMAP     DC    CL4'OMAP'
KMAP     DC    CL4'KMAP'
DUPS     DC    CL4'DUPS'
EFDP     DC    CL4'EFDP'                                          @010I
*
NOTFND   DC    C'NOTFND'
MESSAGE  DC    C'MESSAGE'
CONTINU  DC    C'CONTINUE'
ABEND    DC    C'ABEND'
DUMP     DC    C'DUMP'
NODUMP   DC    C'NODUMP'
LRBUFFER DC    C'LRBUFFER'
EXTENSON DC    C'EXTENSION'
BREAKLEN DC    C'BREAKLEN'
EXTRACT  DC    C'EXTRACT'
IGNORDDN DC    C'IGNOREDDN'
PADDECMP DC    C'PADDECOMP'
BUFREXIT DC    C'BUFFEREXIT'
BUFNO    DC    C'BUFNO'
PTRTBLSZ DC    C'PTRTBLSIZE'                                      @020I
*
*        The following 2 areas are replacements for the old input
*        parameters of LRBUFFER and EXTENSION.
*        Previously LRBUFFER was below "BAR" storage for the inputed
*        records, included in this are was a 32k copy area for copying
*        an above the "bar" record to below the "bar" so we always
*        passed block back to GVBMR95 that was in 31 bit storage.
*        Also in this area was a 32k buffer for records from a file
*        type of "T". With GVBMR95 now running in 64 bit mode we have
*        now made LRBUFFER and EXTENSION into 1 buffer that resides
*        above the "bar" and this is set by the parameter BUFSIZE. The
*        "T" file type records are stored in a buffer set by the
*        TBUFSIZE parameter and is now a buffer by itself. For
*        compatibility if the user still specifies LRBUFFER and
*        EXTENSION but not BUFSIZE then BUFSIZE will be set to the
*        total of LRBUFFER and EXTENSION otherwise the values will be
*        ignored.
*
BUFSIZE  DC    C'BUFSIZE'
TBUFSIZE DC    C'TBUFSIZE'
*
Release_code DC C'042'
*
GENERATE DC    CL19'GENERATED TRANSACT'
*
COUNTMSK DC    XL15'4020206B2020206B2020206B202120'
LENGMSK  DC    XL07'4020206B202120'
BYTEMSK  DC    XL20'402020206B2020206B2020206B2020206B202120'
*
         ORG   *-240              TRANSLATE TABLE FOR BINARY TO HEX
XTAB     EQU   *                       (VIRTUAL ORIGIN)
         ORG   *+240
         DC    CL16'0123456789ABCDEF'
*
*  DATA SOURCE AREA ID     ("CDEXAREA")
SKAREA   EQU   01                      SORT   KEY  AREA
TKAREA   EQU   02                      TITLE  KEY  AREA
DTAREA   EQU   03                      DATA  "DT"  AREA
CTAREA   EQU   04                      CALC  "CT"  AREA
*
TOKNLVL1 DC    A(1)                    NAME/TOKEN  AVAILABILITY  LEVEL
TOKNLVL2 DC    A(2)                    NAME/TOKEN  AVAILABILITY  LEVEL
GENEVA   DC    CL8'GENEVA'                  TOKEN  NAME
GVBMR95  DC    CL8'GVBMR95'
LPGMNAME DS   0CL8                     MINOR  ENQ  NAME (PROGRAM)
EXTNAME  DC    CL16'GVBXRCK LRBUFFER'  MINOR  ENQ  NAME (EXTENSION)
TKNNAME  DC    CL08'MERGXRCK'          MINOR  ENQ  NAME (PROGRAM)
*
GENCKEPT DC    CL8'GENCKTKN'           TOKEN        ENTRY POINT
*
GENCKEPD DC    CL8'GENCKDIR'           DIRECTORY    ENTRY POINT
*
GENCKEPS DC    CL8'GENCKSB'            START BROWSE ENTRY POINT
GENCKEPB DC    CL8'GENCKBR'            BROWSE       ENTRY POINT
GENCKEPG DC    CL8'GENCKTRN'           GENERATE     ENTRY POINT
GENCKEPV DC    CL8'GENCKVRT'           GENERATE     ENTRY POINT
*
InitText DC    C'Initialise global'
InitPText DC   C'Initialise print'
CloseText DC   C'Close entity files'
*
GVBHDRA  DC    V(GVBUTHDR)
IEANTRT  DC    V(IEANTRT)
IEANTCR  DC    V(IEANTCR)
IEA4XFR  DC    V(IEA4XFR)
IEA4DPE  DC    V(IEA4DPE)
*
*
RPTFILEA DC    A(RPTFILE)
INIFILEA DC    A(INIFILE)
PRMFILEA DC    A(PRMFILE)
*
TRTTBLUA DC    A(TRTTBLU)          UNPACKED NUMERIC TRT TABLE
*
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V A R I A B L E S                                            *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         PRINT nogen
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        D A T A   C O N T R O L   B L O C K S                        *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
RPTFILE  DCB   DSORG=PS,DDNAME=MERGRPT,MACRF=(PM),DCBE=RPTDCBE,        X
               RECFM=FBA,LRECL=133
RPTEOFF  EQU   *-RPTFILE
RPTDCBE  DCBE  RMODE31=BUFF
RPTFILEL EQU   *-RPTFILE
*
PRMFILE  DCB   DSORG=PS,DDNAME=MERGPARM,MACRF=(GL),DCBE=PRMDCBE
PRMEOFF  EQU   *-PRMFILE
PRMDCBE  DCBE  RMODE31=BUFF,EODAD=INITPEOF
PRMFILEL EQU   *-PRMFILE
*
*
INIFILE  DCB   DSORG=PS,DDNAME=MERGINIT,MACRF=(GL),DCBE=INIDCBE
INIEOFF  EQU   *-INIFILE
INIDCBE  DCBE  RMODE31=BUFF,EODAD=INITIEOF
INIFILEL EQU   *-INIFILE
*
VDPFILE  DCB   DSORG=PS,DDNAME=MERGVDP,MACRF=(GL),DCBE=VDPDCBE
VDPDCBE  DCBE  RMODE31=BUFF,EODAD=VDPEOF
VDPFILEL EQU   *-VDPFILE
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
XHEXFF   DC 1024X'FF'
code     loctr
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        I N I T I A L I Z E   W O R K   A R E A                      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING GENPARM,R9
*   Note INITWORK operates in 31 bit mode
         sysstate amode64=NO
*
INITWORK ST    R9,WKPLISTA        SAVE  PARAMETER  LIST   ADDRESS
*
         TCBTOKEN TYPE=CURRENT,TTOKEN=TCBTOKEN,MF=(E,REENTWK)
*
*
         L     R0,F10MEG          DEFAULT RECORD PTR TABLE SIZE   @020I
         ST    R0,WKLSTSIZ                                        @020I
*                                                                 @020I
         MVC   WKCALLPL(WKCALLEN),0(R9)  COPY READ EXIT PARAM LIST
         LA    R0,WKRXITRC        RETURN     CODE
         ST    R0,WKCALLP8
         LA    R0,WKCALLPT        RETURNED RECORD  POINTER
         ST    R0,WKCALLP9
         LA    R0,WKCALLRL        RETURNED RECORD  LENGTH
         ST    R0,WKCALLPA
*
         LHI   R0,L'WKKEYSV1
         ST    R0,WKSAVLEN
*
         LAY   R0,WKKEYSV1
         ST    R0,WKBRKLOW
         LAY   R0,WKKEYSV2
         ST    R0,WKBRKID
         LAY   R0,WKKEYSV3
         ST    R0,WKBRKOLD
*
         llgt  R0,WKBRKID         INITIALIZE CURRENT BREAK ID
         lghi  R1,L'WKKEYSV1
         sgr   R14,R14
         sgr   R15,R15
         MVCL  R0,R14
*
         ZAP   WKPRTCNT,P000      ZERO  PARTITION  RECORD   COUNT
         ZAP   WKPRTDUP,P000      ZERO  PARTITION  DUPLICATE  REC
         ZAP   WKPRTBYT,P000      ZERO  PARTITION  BYTE     TOTAL
*
         xc    WKGENCNT,WKGENCNT  ZERO  GENERATED  TRANSACTION COUNT
         ZAP   WKGENBYT,P000      ZERO  GENERATED  TRANSACTION BYTES
*
         XC    WKKEYLN,WKKEYLN     RTC19235
*
         LAY   R14,MDLWTO         INITIALIZE  WTO  PARAMETER AREA
         MVC   WKWTOPRM(WKWTOLEN),0(R14)
*
         LA    R14,WKTIOTA
         XC    REENTWK,REENTWK    ZERO THE EXTRACT PARAMETER LIST AREA
         EXTRACT (R14),'S',FIELDS=TIOT,MF=(E,REENTWK)
*
*
***********************************************************************
*  SAVE DDNAME AND SET EVENT FILE ATTRIBUTES                          *
***********************************************************************
         llgt  R14,GPFILEA        EVENT FILE DDNAME
         USING GENFILE,R14
         MVC   WKDDNAME,GPDDNAME
         MVC   WKLFID,GPLFID
*
         MVI   GPRECFMT,C'V'      VARIABLE   FORMAT   RECORDS
         XC    GPRECDEL,GPRECDEL
*
         llgt  R1,GPBLKSIZ
         L     R0,0(,R1)
         ST    R0,GPBLKMAX
         AHI   R0,-4
         ST    R0,GPRECMAX
         DROP  R14
*
*
***********************************************************************
*  SAVE STARTING DATE/TIME                                            *
***********************************************************************
         LA    R2,WKTIMWRK
         TIME  BIN,(2),LINKAGE=SYSTEM,DATETYPE=YYYYMMDD,MF=(E,REENTWK)
         MVC   WKBEGTIM,WKTIMWRK+0
         MVO   WKBEGDAT,WKTIMWRK+8(4)
         OI    WKBEGDAT+L'WKBEGDAT-1,X'0F'
                        EJECT
***********************************************************************
* CREATE NAME/TOKEN FOR SAVING GLOBAL DATA FOR "GVBXRCK"(ALL THREADS)*
***********************************************************************
INITENQ  MVC   REENTWK(MDLENQPL),MDLENQP
         enq   MF=(E,REENTWK)
         LTR   R15,R15
         BRZ   INITGLOB
*
         GVBMSG FORMAT,MSGNO=ENQ_FAIL,SUBNO=3,                         +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(LPGMNAME,l'LPGMNAME),                             +
               SUB3=(INITTEXT,l'INITTEXT),  initialization             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
         BRU   RTNERROR
*
INITGLOB MVC   WKTOKNAM+0(8),GENEVA
         MVC   WKTOKNAM+8(8),TKNNAME
*
         llgt  R14,GPSTARTA       LOAD  START-UP  DATA   ADDRESS
         USING STRTDATA,R14
         if CLC,SDDDNPFX,NE,BLANKS  CHECK IF DDNAME PREFIX SPECIFIED
           MVC WKTOKNAM+8(4),SDDDNPFX
         endif
         DROP  R14
*
         CALL  IEANTRT,(TOKNLVL2,WKTOKNAM,WKTOKN,WKTOKNRC),            X
               MF=(E,REENTWK)
         ltgf  R15,WKTOKNRC       SUCCESSFUL   ???
         BRNZ  INITGLBL           NO  - CREATE THE FIRST ONE
*
         llgt  R4,WKTOKN+0        RETRIEVE  GLOBAL AREA  ADDRESS
         ST    R4,WKGLOBA
         USING GLOBAREA,R4
*
*        Load all entry points and modules here so that the use
*        count is incremented (to avoid loss of the entry point when
*        the task that did the identify or initial load finishes)
*
         LOAD  EP=GVBUR20         Do again to increment use count pgc01
         LOAD  EP=GVBUR30
         LOAD  EP=GVBUR35
         LOAD  EP=GENPIPE
*        include the IDENTIFYed points as well
***********************************************************************
*  DYNAMICALLY ESTABLISH GENEVA SUBROUTINE ENTRY POINTS               *
***********************************************************************
         LAY      R0,GENCKEPT                                   @I10154
         larl     R1,GENCKTKN                                   @I10154
         IDENTIFY EPLOC=(0),ENTRY=(1)                           @I10154
*                                                               @I10154
         LAY      R0,GENCKEPD                                   @I10154
         larl     R1,GENCKDIR                                   @I10154
         IDENTIFY EPLOC=(0),ENTRY=(1)                           @I10154
*                                                               @I10154
         LAY      R0,GENCKEPS                                   @I10154
         larl     R1,GENCKSB                                    @I10154
         IDENTIFY EPLOC=(0),ENTRY=(1)                           @I10154
*                                                               @I10154
         LAY      R0,GENCKEPB                                   @I10154
         larl     R1,GENCKBR                                    @I10154
         IDENTIFY EPLOC=(0),ENTRY=(1)                           @I10154
*                                                               @I10154
         LAY      R0,GENCKEPG                                   @I10154
         larl     R1,GENCKTRN                                   @I10154
         IDENTIFY EPLOC=(0),ENTRY=(1)                           @I10154
*                                                               @I10154
         LAY      R0,GENCKEPV                                   @I10154
         larl     R1,GENCKVRT                                   @I10154
         IDENTIFY EPLOC=(0),ENTRY=(1)                           @I10154
                        EJECT
***********************************************************************
*  LOAD  DYNAMIC SUBROUTINES                                          *
***********************************************************************
         LOAD  EP=GVBUR20         SEQUENTIAL  FILE  I/O          @I1015
         Oilh  R0,MODE31                                         @I1015
         ST    R0,GLOBUR20                                       @I1015
         ST    R0,GVBUR20                                        @I1015
*                                                                @I1015
         LOAD  EP=GVBUR30         DB2   ACCESS                   @I1015
         Oilh  R0,MODE31                                         @I1015
         ST    R0,GLOBUR30                                       @I1015
         ST    R0,GVBUR30                                        @I1015
*                                                                @I1015
         LOAD  EP=GVBUR35         DYNAMIC FILE ALLOCATION        @I1015
         Oilh  R0,MODE31                                         @I1015
         ST    R0,GLOBUR35                                       @I1015
         ST    R0,GVBUR35                                        @I1015
*                                                                @I1015
         LOAD  EP=GENPIPE         READ  GENEVA PIPE              @I1015
         ST    R0,GLOBGENP                                       @I1015
         ST    R0,GENPIPE                                        @I1015
*
         MVC   WKPARMA,GLOBPARM
*
* Load exits again so use count is incremented
*
         llgt  r2,wkparma         Get parameter table
         Using parmtbl,r2         Set base
         do until=(cli,parment,eq,X'ff')     test at end of loop
           if cli,parmkxit,ne,c' ' is it non blank?
             LOAD EPLOC=parmkxit
           endif
           if cli,parmrxit,ne,c' ' is it non blank?
             LOAD EPLOC=parmrxit
           endif
           if cli,parmpxit,ne,c' ' is it non blank?
             LOAD EPLOC=parmpxit
           endif
           aghi r2,parmlen         Point at next entry
         enddo ,
         drop  r2

*
         if cli,globbxit,ne,c' ' is it non blank?
           LOAD EPLOC=globbxit
         endif
*
         MVC   WKRINITB,GLOBRINB
         MVC   WKRINITM,GLOBRINM
         MVC   WKRINITE,GLOBRINE
*
         LH    R0,GLOBECNT        SAVE ENTITY    COUNT
         ST    R0,WKENTCNT
         LH    R0,GLOBFCNT        SAVE FILE AREA COUNT
         ST    R0,WKFILCNT
*                                                                 @020I
         LH    R0,GLOBKEYM        SAVE MAX  KEY  LENGTH           @020I
         STH   R0,WKMAXKEY                                        @020I
*                                                                 @020I
         LH    R0,GLOBBRKL        SAVE BREAK     LENGTH
         STH   R0,WKBRKLEN
*
         if cij,r0,lt,0           NEGATIVE  ???
           MVI WKEXTFLG,C'Y'      YES - "BREAKLEN=EXTRACT" SPECIFIED
         endif
         LH    R0,GLOBNBUF        SAVE "BUFNO"
         if cij,r0,lt,0           NEGATIVE  ???
           LHI R0,FILENBUF
         endif
         ST    R0,WKBUFNO
*                                                                 @020I
         LH    R0,GLOBMPTR        LOAD "PTRTBLSIZE" PARAMETER     @020I
         if cij,r0,ge,0           Positive  ???
           SLL R0,20              CONVERT TO MEGABYTES            @020I
           ST  R0,WKLSTSIZ                                        @020I
         endif
*
         MVC   WKIGNRDD,GLOBIDDN
         MVC   WKPADCMP,GLOBDCMP
*
         MVC   WKVWLIST,GLOBVLST
*
         MVC   WKBXITEP,GLOBBXEP
         MVC   WKBXITCA,GLOBBXCA
         MVC   WKBXITPM,GLOBBXPM
*
         BRU   INITTOKN
*
***********************************************************************
* INITIALIZE FIRST GLOBAL AREA                                        *
***********************************************************************
INITGLBL XC    WKTOKN,WKTOKN
*
         LHI   R0,GLOBLEN+8
         GETMAIN RU,LV=(0),LOC=(ANY)
         MVC   0(8,R1),GLOBEYEB
         AHI   R1,8
         ST    R1,WKGLOBA
         ST    R1,WKTOKN+0
         LR    R4,R1
         USING GLOBAREA,R4
*
         XC    0(GLOBLEN,R4),0(R4)  ZERO "GVBXRCK" GLOBAL   AREA
*
         mvc   globbxit,blanks      default is no buffer exit
*
         Llgt  R14,GPENVA
         LLGT  R0,GP_PF_COUNT-GENENV(,R14)
         STH   R0,GLOBPART          Total PF count
         CVD   R0,WKDBLWRK
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         UNPK  WKDBLWK2,WKDBLWRK
*
         GVBMSG LOG,MSGNO=CK_NUM_PARTITIONS,SUBNO=2,                   +
               GENENV=(R14),                                           +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(WKDBLWK2,l'WKDBLWK2),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*        WTO   TEXT=WKTXTLEN,MF=(E,WKWTOPRM)
*
         ZAP   GLOBREAD,P000        ZERO CUMULATIVE COUNTS
         ZAP   GLOBBYTE,P000
         ZAP   GLOBDUPE,P000
         ZAP   GLOBFNDE,P000
         ZAP   GLOBFNDT,P000
         ZAP   GLOBFNDX,P000
         ZAP   GLOBFNDR,P000
         ZAP   GLOBFNDB,P000
         ZAP   GLOBNOTE,P000
         ZAP   GLOBNOTT,P000
         ZAP   GLOBNOTX,P000
         ZAP   GLOBNOTR,P000
         ZAP   GLOBNOTB,P000
*
         MVI   GLOBPRNT,C'Y'        SET  FIRSTTIME  PRINT   INDICATOR
         MVI   GLOBLKUP,C'A'        ABEND     WHEN  LOOK-UP NOT FOUND
         ZAP   GLOBBRKC,P000
         ZAP   GLOBMDSW,P000
*
         MVC   GLOBMPTR,XHEXFF      SET  POINTER TABLE MEGABYTES TO -1
*
*
***********************************************************************
*  LOAD  DYNAMIC SUBROUTINES                                          *
***********************************************************************
         LOAD  EP=GVBUR20         SEQUENTIAL  FILE  I/O
         Oilh  R0,MODE31
         ST    R0,GLOBUR20
         ST    R0,GVBUR20
*
         LOAD  EP=GVBUR30         DB2   ACCESS
         Oilh  R0,MODE31
         ST    R0,GLOBUR30
         ST    R0,GVBUR30
*
         LOAD  EP=GVBUR35         DYNAMIC FILE ALLOCATION
         Oilh  R0,MODE31
         ST    R0,GLOBUR35
         ST    R0,GVBUR35
*                                                                 @017I
         LOAD  EP=GENPIPE         READ  GENEVA PIPE               @017I
         ST    R0,GLOBGENP                                        @017I
         ST    R0,GENPIPE                                         @017I
                        SPACE 3
***********************************************************************
*  DYNAMICALLY ESTABLISH GENEVA SUBROUTINE ENTRY POINTS               *
***********************************************************************
         LAY      R0,GENCKEPT
         larl     R1,GENCKTKN
         IDENTIFY EPLOC=(0),ENTRY=(1)
*
         LAY      R0,GENCKEPD
         larl     R1,GENCKDIR
         IDENTIFY EPLOC=(0),ENTRY=(1)
*
         LAY      R0,GENCKEPS
         larl     R1,GENCKSB
         IDENTIFY EPLOC=(0),ENTRY=(1)
*
         LAY      R0,GENCKEPB
         larl     R1,GENCKBR
         IDENTIFY EPLOC=(0),ENTRY=(1)
*
         LAY      R0,GENCKEPG
         larl     R1,GENCKTRN
         IDENTIFY EPLOC=(0),ENTRY=(1)
*
         LAY      R0,GENCKEPV
         larl     R1,GENCKVRT
         IDENTIFY EPLOC=(0),ENTRY=(1)
                        EJECT
***********************************************************************
*  LOAD  PARAMETER AREA                                               *
***********************************************************************
INITPARM LHI   R0,PRMFILEL        LOAD DCB  LENGTH
         GETMAIN RU,LV=(0),LOC=(BELOW) GET  MEMORY FOR DCB
         ST    R1,WKPRMDCB        SAVE DCB ADDRESS IN  CONTROL ELEMENT
         lgr   R2,R1
         USING IHADCB,R2
*
         LAY   R14,PRMFILE             COPY  DCB
         MVC   0(PRMFILEL,R2),0(R14)
*
         LAY   R14,PRMDCBE-PRMFILE(,R2)
         ST    R14,DCBDCBE
*
         llgt  R14,GPSTARTA       LOAD  START-UP  DATA   ADDRESS
         USING STRTDATA,R14
         if CLC,SDDDNPFX,ne,BLANKS  CHECK IF DDNAME PREFIX SPECIFIED
           MVC DCBDDNAM-IHADCB(L'SDDDNPFX,R2),SDDDNPFX
         endif
         DROP  R14
*
         MVC   REENTWK(8),OPENPARM
         OPEN  ((R2),INPUT),MODE=31,MF=(E,REENTWK)  OPEN FILE
         TM    48(R2),X'10'                         SUCCESSFUL ???
         BRO   INITPRM
*
         GVBMSG FORMAT,MSGNO=OPEN_MERGPRM_FAIL,SUBNO=2,                +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(DCBDDNAM,l'DCBDDNAM),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
         BRU   RTNERROR
*
         DROP  R2
*
INITPRM  LA    R0,MAXPARM         LOAD  NUMBER OF ENTITIES (MAXIMUM)
         LA    R1,PARMLEN         LOAD  LENGTH
         MR    R0,R0              COMPUTE AREA SIZE
         lgr   R2,R1              SAVE    AREA SIZE
*
         lgr   R0,R1
         GETMAIN RU,LV=(0),LOC=(ANY)
         ST    R1,GLOBPARM        SAVE  PARAMETER AREA ADDRESS
         ST    R1,WKPARMA
*
         llgt  R0,WKPARMA         INITIALIZE  AREA TO X'FF'
         lgr   R1,R2
         sgr   R14,R14
         sgr   R15,R15
         ICM   R15,B'1000',XHEXFF
         MVCL  R0,R14
*
         llgt  R2,WKPRMDCB        LOAD PARM FILE DCB ADDRESS
*
         LA    R3,MAXPARM         LOAD MAXIMUM  NUMBER OF    ENTRIES
         LA    R5,XHEXFF          INITIALIZE PREVIOUS  ENTRY ADDRESS
         llgt  R7,WKPARMA         LOAD FIRST PARAMETER ENTRY ADDRESS
         USING PARMTBL,R7
*
         mvi   WKTYPSW,c'F'       first time indicator RTC19235
initprml llgt  R1,WKPRMDCB        LOAD PARM FILE DCB ADDRESS
         GET   (1)                READ NEXT  PARAMETER RECORD
         llgtr r11,r1
         USING CARDPARM,R11
*
         CLI   CARDENT,C'*'               COMMENT  ???
         BRE   INITPRML                   YES - IGNORE
*
***********************************************************************
*  CHECK FOR KEYWORDS                                                 *
***********************************************************************
         if CLC,CARDENT(L'NOTFND),eq,NOTFND LOOK-UP  NOT FOUND OPTION?
*
         select ,
           when CLC,CARDRSRC(L'MESSAGE),eq,MESSAGE
             MVI GLOBLKUP,C'M'            MESSAGE

           when CLC,CARDRSRC(L'CONTINU),eq,CONTINU
             MVI GLOBLKUP,C'C'            CONTINUE

           when CLC,CARDRSRC(L'ABEND),eq,ABEND
             MVI GLOBLKUP,C'A'            ABEND - NODUMP

           when CLC,CARDRSRC(L'NODUMP),eq,NODUMP
             MVI GLOBLKUP,C'A'            ABEND - NODUMP

           when CLC,CARDRSRC(L'DUMP),eq,DUMP
             MVI GLOBLKUP,C'D'            ABEND -   DUMP

           endsel
           MVC PARMTBL(PARMLEN),BLANKS
           MVC PARMENT(L'NOTFND),CARDENT  COPY  "NOTFND"
           MVC PARMRSRC,CARDRSRC          COPY  LOOK-UP NOT FOUND OPT
*
           BRU INITPNXT                   GET   NEXT PARAMETER RECORD
*
         endif
***********************************************************************
*  EDIT BREAK KEY LENGTH                                              *
***********************************************************************
INITBLEN CLC   CARDENT(L'BREAKLEN),BREAKLEN "KEY  BREAK LENGTH"   ???
         BRNE  INITBSIZ                      NO - check for BUFSIZE
*
         CLC   CARDRSRC(L'EXTRACT),EXTRACT
         BRNE  INITBNUM
*
         MVI   WKEXTFLG,C'Y'
         MVC   WKBRKLEN,XHEXFF
         MVC   PARMTBL(PARMLEN),BLANKS
         MVC   PARMENT(L'BREAKLEN),CARDENT   COPY "BREAKLEN"
         MVC   PARMRSRC(L'EXTRACT),CARDRSRC  COPY "EXTRACT"
*
         BRU   INITPNXT
*
INITBNUM LAY   R14,TRTTBLU        LOAD  NUMERIC  CLASS TEST  TBL  ADDR
         CLI   CARDRSRC+2,C' '
         BRNE  INITBKY3
         TRT   CARDRSRC(2),0(R14) NUMERIC  ???
         BRZ   INITBKYL
         BRU   INITBKYE
*
INITBKY3 TRT   CARDRSRC(3),0(R14) NUMERIC  ???
         BRZ   INITBKYL           YES - CONTINUE
*
INITBKYE DS    0H                 DISPLAY "BAD KEY LENGTH"   MESSAGE
         GVBMSG FORMAT,MSGNO=CK_PARM_NOT_NUM,SUBNO=2,                  +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(BREAKLEN,L'BREAKLEN),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR
*
INITBKYL PACK  WKDBLWRK,CARDRSRC(2)
         if CLI,CARDRSRC+2,ne,C' '
           PACK WKDBLWRK,CARDRSRC(3)
         endif
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         CVB   R0,WKDBLWRK
         STH   R0,WKBRKLEN
*
         MVC   PARMTBL(PARMLEN),BLANKS
         MVC   PARMENT(L'BREAKLEN),CARDENT   COPY "BREAKLEN"
         MVC   PARMRSRC(3),CARDRSRC          COPY BUFFER SIZE
*
         BRU   INITPNXT                      GET NEXT PARAMETER RECORD
*
***********************************************************************
*  EDIT "BUFSIZE" size                                                *
***********************************************************************
INITbSIZ CLC   CARDENT(L'bufsize),bufsize buffer SIZE (MEG) ???
         jne   INITtsiz                     NO - CHECK for LRBUFFER
*
         PACK  WKDBLWRK,CARDRSRC(3)
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         CVB   R0,WKDBLWRK
         st    R0,globbsiz
*
         MVC   PARMTBL(PARMLEN),BLANKS
         MVC   PARMENT(L'LRBUFFER),CARDENT   COPY "LRBUFFER"
         MVC   PARMRSRC(3),CARDRSRC          COPY BUFFER SIZE
*
         BRU   INITPNXT                   GET   NEXT  PARAMETER RECORD
*
***********************************************************************
*  EDIT "TBUFSIZE"  SIZE                                              *
***********************************************************************
INITtSIZ CLC   CARDENT(L'tbufsize),tbufsize buffer SIZE (MEG) ???
         jne   INITlrsiz                     NO - CHECK for LRBUFFER
*
         PACK  WKDBLWRK,CARDRSRC(3)
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         CVB   R0,WKDBLWRK
         st    R0,globtsiz
*
         MVC   PARMTBL(PARMLEN),BLANKS
         MVC   PARMENT(L'LRBUFFER),CARDENT   COPY "LRBUFFER"
         MVC   PARMRSRC(3),CARDRSRC          COPY BUFFER SIZE
*
         BRU   INITPNXT                   GET   NEXT  PARAMETER RECORD
*
***********************************************************************
*  EDIT "LR" BUFFER SIZE                                              *
***********************************************************************
INITlrSIZ CLC   CARDENT(L'LRBUFFER),LRBUFFER "LR BUFFER" SIZE (MEG) ???
         BRNE  INITXSIZ                      NO - CHECK EXTENSION SIZE
*
         PACK  WKDBLWRK,CARDRSRC(3)
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         CVB   R0,WKDBLWRK
         sty   R0,lrbsize
*
         MVC   PARMTBL(PARMLEN),BLANKS
         MVC   PARMENT(L'LRBUFFER),CARDENT   COPY "LRBUFFER"
         MVC   PARMRSRC(3),CARDRSRC          COPY BUFFER SIZE
*
         BRU   INITPNXT                   GET   NEXT  PARAMETER RECORD
*
***********************************************************************
*  EDIT EXTENSION BUFFER SIZE                                         *
***********************************************************************
INITXSIZ CLC   CARDENT(L'EXTENSON),EXTENSON "LR BUFFER EXTENSION"  ???
         BRNE  INITBUFN                      NO -  CHECK  "BUFNO"
*
         if CLI,CARDRSRC+3,ne,C' '               4  DIGIT  NUMBER ??
           PACK WKDBLWRK,CARDRSRC(4)
         else
           PACK WKDBLWRK,CARDRSRC(3)
         endif
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         CVB   R0,WKDBLWRK
         sty   R0,lrxsize
*
         MVC   PARMTBL(PARMLEN),BLANKS
         MVC   PARMENT(L'EXTENSON),CARDENT   COPY  "EXTENSION"
         MVC   PARMRSRC(3),CARDRSRC          COPY  "EXTENSION"  SIZE
*
         BRU   INITPNXT                   GET   NEXT  PARAMETER RECORD
*
***********************************************************************
*  EDIT "BUFNO"                                                       *
***********************************************************************
INITBUFN CLC   CARDENT(L'BUFNO),BUFNO       "BUFNO" ???
         BRNE  INITPTBL                      NO - COPY VARIABLES  @020C
*
         LAY   R14,TRTTBLU        LOAD  NUMERIC  CLASS TEST  TBL  ADDR
         CLI   CARDRSRC+2,C' '
         BRNE  INITBUF3
         TRT   CARDRSRC(2),0(R14) NUMERIC  ???
         BRZ   INITBUFL
         BRU   INITBUFE
*
INITBUF3 TRT   CARDRSRC(3),0(R14) NUMERIC  ???
         BRZ   INITBUFL           YES - CONTINUE
*
INITBUFE DS    0h                 NON-NUMERIC "BUFNO" MESSAGE
         GVBMSG FORMAT,MSGNO=CK_PARM_NOT_NUM,SUBNO=2,                  +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(BUFNO,L'BUFNO),                                   +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR
*
INITBUFL PACK  WKDBLWRK,CARDRSRC(2)
         if CLI,CARDRSRC+2,ne,C' '
           PACK WKDBLWRK,CARDRSRC(3)
         endif
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         CVB   R0,WKDBLWRK
         STH   R0,GLOBNBUF                                        @020I
         ST    R0,WKBUFNO                                         @020C
         if cij,r0,le,1               "BUFNO" must be > 1
*
           GVBMSG FORMAT,MSGNO=CK_BUFNO_ERR2,SUBNO=1,                  +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
           j   rtnerror
         endif
*
         MVC   PARMTBL(PARMLEN),BLANKS
         MVC   PARMENT(L'BUFNO),CARDENT      COPY "BUFNO"
         MVC   PARMRSRC(3),CARDRSRC          COPY  BUFFER  SIZE
*
         BRU   INITPNXT                      GET NEXT PARAMETER RECORD
*
******************************************************************@020I
*  EDIT "PTRTBLSIZE"                                              @020I
******************************************************************@020I
INITPTBL CLC   CARDENT(L'PTRTBLSZ),PTRTBLSZ "PTRTBLSIZE"   ???    @020I
         BRNE  INITIGNR                      NO - COPY VARIABLES  @020I
*                                                                 @020I
         llgt  R14,TRTTBLUa       LOAD NUMERIC CLASS TEST TBL ADDR@020I
         TRT   CARDRSRC(2),0(R14) NUMERIC  ???                    @020I
         BRZ   INITPTRS           YES - CONVERT TO  BINARY        @020I
*                                                                 @020I
*        "BAD TABLE SIZE" MESSAGE
         GVBMSG FORMAT,MSGNO=CK_PARM_NOT_NUM,SUBNO=2,                  +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(PTRTBLSZ,L'PTRTBLSZ),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR                                           @020I
*                                                                 @020I
INITPTRS PACK  WKDBLWRK,CARDRSRC(2)                               @020I
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'                        @020I
         CVB   R0,WKDBLWRK                                        @020I
         STH   R0,GLOBMPTR                                        @020I
         SLL   R0,20              CONVERT TO MEGABYTES            @020I
         ST    R0,WKLSTSIZ                                        @020I
*                                                                 @020I
         MVC   PARMTBL(PARMLEN),BLANKS                            @020I
         MVC   PARMENT(L'PTRTBLSZ),CARDENT COPY "PTRTBLSIZE"      @020I
         MVC   PARMRSRC(2),CARDRSRC        COPY  TABLE   SIZE     @020I
*                                                                 @020I
         BRU   INITPNXT           GET NEXT PARAMETER RECORD       @020I
*
***********************************************************************
*  EDIT "IGNORE MISSING DD STATEMENT" OPTION                          *
***********************************************************************
INITIGNR CLC   CARDENT(L'IGNORDDN),IGNORDDN "IGNORE MISSING DDN"  ???
         BRNE  INITDCMP                      NO - COPY VARIABLES
*
         MVI   GLOBIDDN,C'Y'
         MVI   WKIGNRDD,C'Y'
*
         MVC   PARMTBL(PARMLEN),BLANKS
         MVC   PARMENT(L'IGNORDDN),CARDENT   COPY  "IGNORE MISSING DDN"
*
         BRU   INITPNXT                   GET   NEXT  PARAMETER RECORD
*
***********************************************************************
*  EDIT NUMERIC PAD DECOMPRESSION OPTION/RE-POPULATE MISSING "CT" COLS*
***********************************************************************
INITDCMP CLC   CARDENT(L'PADDECMP),PADDECMP  DECOMPRESS "CT" COLS ???
         BRNE  INITBXIT                      NO - COPY VARIABLES
*
         MVI   GLOBDCMP,C'Y'
         MVI   WKPADCMP,C'Y'
*
         MVC   PARMTBL(PARMLEN),BLANKS
         MVC   PARMENT(L'PADDECMP),CARDENT   COPY  "ACCEPT DUPLICATES"
*
         BRAS  R14,VIEWLOAD
*
         MVC   GLOBVLST,WKVWLIST  SAVE  DECOMPRESSED   VIEW  LIST ADDR
*
         BRU   INITPNXT           GET   NEXT PARAMETER RECORD
*
***********************************************************************
*  EDIT "BUFFER READY" EXIT PROGRAM NAME/ADDRESS                      *
***********************************************************************
INITBXIT CLC   CARDENT(L'BUFREXIT),BUFREXIT "BUFFER READY EXIT"   ???
         BRNE  INITPENT                      NO - COPY VARIABLES
*
         MVC   GLOBBXIT,CARDXNAM
         MVC   GLOBBXPM,CARDXPRM
*
         MVC   PARMTBL(PARMLEN),BLANKS
         MVC   PARMENT(L'BUFREXIT),CARDENT   COPY "BUFFER EXIT NAME"
         MVC   PARMRSRC(L'GLOBBXIT),GLOBBXIT
         MVC   PARMRSRC+L'PARMRSRC+1(L'GLOBBXPM),GLOBBXPM
*
         LA    R0,GLOBBXIT                EXIT  NAME
         LA    R1,GLOBBXEP                EXIT  ADDRESS
         BRAS  R14,EXITLOAD
*
         LAY   R0,CALLEXIT        ASSUME COBOL
         Oilh  R0,MODE31
         llgf  R1,GLOBBXEP        CEE   SUBROUTINE   ???
         if CLC,05(3,R1),ne,LECOBOL
           LR  R0,R1              SUBSTITUTE COMMON  LANG INTERFACE
         endif
         ST    R0,GLOBBXCA
*
         MVC   WKBXITEP,GLOBBXEP
         MVC   WKBXITCA,GLOBBXCA
         MVC   WKBXITPM,GLOBBXPM
*
         BRU   INITPNXT                   GET   NEXT  PARAMETER RECORD
*
***********************************************************************
*  PROCESS ENTITIES                                                   *
***********************************************************************
INITPENT CLC   CARDENT,PARMENT-PARMTBL(R5)      SAME  ENTITY   ???
         BRNE  INITPCNT                   NO  - INCREMENT      COUNT
*
         CLC   CARDKEYW(L'DSN),DSN        DATASET     NAME ???
         BRNE  INITECMP                   NO  - BRANCH
*
         MVC   PARMDSN-PARMTBL(L'PARMDSN,R5),CARDDSN
         BRU   INITPRML                   READ  NEXT  PARM
*
INITECMP CLC   CARDKEYW(L'DCMP),DCMP      ENTITY DECOMPRESS ???
         BRNE  INITOMAP                   NO  - BRANCH
*
         MVI   PARMDCMP-PARMTBL(R5),C'Y'
*
         BRAS  R14,VIEWLOAD
*
         MVC   GLOBVLST,WKVWLIST  SAVE  DECOMPRESSED   VIEW  LIST ADDR
*
         BRU   INITPRML                   READ  NEXT  PARM
*
INITOMAP CLC   CARDKEYW(L'OMAP),OMAP      OUTPUT CATEGORY MAP  ???
         BRNE  INITKMAP                   NO  - BRANCH
*
         MVC   PARMOMAP-PARMTBL(L'PARMOMAP,R5),CARDOMAP
         BRU   INITPRML                   READ  NEXT  PARM
*
INITKMAP CLC   CARDKEYW(L'KMAP),KMAP      OUTPUT CATEGORY MAP  ???
         JNE   INITKXIT                   NO  - BRANCH            @008C
*
         MVC   PARMKMAP-PARMTBL(L'PARMKMAP,R5),CARDOMAP
         BRU   INITPRML                   READ  NEXT  PARM
*
INITKXIT CLC   CARDKEYW(L'KXIT),KXIT      ENTITY KEY  EXIT ???    @008I
         BRNE  INITRXIT                   NO  - BRANCH            @008I
*                                                                 @008I
         MVC   PARMKXIT-PARMTBL(L'PARMKXIT,R5),CARDXNAM           @008I
         MVC   PARMRXPM-PARMTBL(L'PARMRXPM,R5),CARDXPRM           @008I
*                                                                 @008I
         LA    R0,PARMKXIT-PARMTBL(,R5)   EXIT  NAME              @008I
         LA    R1,PARMKXEP-PARMTBL(,R5)   EXIT  ADDRESS           @008I
         BRAS  R14,EXITLOAD                                       @008I
*                                                                 @008I
         LAY   R0,CALLEXIT              ASSUME  COBOL             @008I
         Oilh  R0,MODE31                                          @008I
         llgf  R1,PARMKXEP-PARMTBL(,R5)    CEE  SUBROUTINE   ???  @008I
         if CLC,05(3,R1),ne,LECOBOL
           LR  R0,R1              SUBSTITUTE COMMON  LANG INTERFACE
         endif
         ST    R0,PARMRXCA-PARMTBL(,R5)                           @008I
*                                                                 @008I
         BRU   INITPRML                   READ  NEXT  PARM        @008I
*                                                                 @008I
INITRXIT CLC   CARDKEYW(L'RXIT),RXIT      ENTITY READ EXIT ???
         BRNE  INITPXIT                   NO  - BRANCH
*
         MVC   PARMRXIT-PARMTBL(L'PARMRXIT,R5),CARDXNAM
         MVC   PARMRXPM-PARMTBL(L'PARMRXPM,R5),CARDXPRM
*
         LA    R0,PARMRXIT-PARMTBL(,R5)   EXIT  NAME
         LA    R1,PARMRXEP-PARMTBL(,R5)   EXIT  ADDRESS
         BRAS  R14,EXITLOAD
*
         LAY   R0,CALLEXIT              ASSUME  COBOL
         Oilh  R0,MODE31
         llgf  R1,PARMRXEP-PARMTBL(,R5)    CEE  SUBROUTINE   ???
         if CLC,05(3,R1),ne,LECOBOL
           LR  R0,R1              SUBSTITUTE COMMON  LANG INTERFACE
         endif
         ST    R0,PARMRXCA-PARMTBL(,R5)
*
         BRU   INITPRML                   READ  NEXT  PARM
*
INITPXIT CLC   CARDKEYW(L'PXIT),PXIT      PROFILE ID  EXIT ???
         BRNE  INITNBLK                   NO  - BRANCH
*
         MVC   PARMPXIT-PARMTBL(L'PARMPXIT,R5),CARDXNAM
         MVC   PARMPXPM-PARMTBL(L'PARMPXPM,R5),CARDXPRM
*
         LA    R0,PARMPXIT-PARMTBL(,R5)   EXIT  NAME
         LA    R1,PARMPXEP-PARMTBL(,R5)   EXIT  ADDRESS
         BRAS  R14,EXITLOAD
*
         LAY   R0,CALLEXIT              ASSUME  COBOL
         Oilh  R0,MODE31
         llgf  R1,PARMPXEP-PARMTBL(,R5) CEE     SUBROUTINE  ???
         if CLC,05(3,R1),ne,LECOBOL
           LR  R0,R1              SUBSTITUTE COMMON  LANG INTERFACE
         endif
         ST    R0,PARMPXCA-PARMTBL(,R5)
*
         BRU   INITPRML                   READ  NEXT  PARM
*
INITNBLK ds    0h
         if CLC,CARDKEYW(L'NEWB),ne,NEWB  ENTITY BEGINS NEW BLOCK ???
           CLC CARDKEYW(L'NBLK),NBLK      ENTITY BEGINS NEW BLOCK ???
           BRNE INITDUPS                  NO  - BRANCH
         endif
*
         MVI   PARMNEWB-PARMTBL(R5),C'Y'
*
         BRU   INITPRML                   READ  NEXT  PARM
*
INITDUPS CLC   CARDKEYW(L'DUPS),DUPS      DUPS - allow duplicate keys
         JNE   INITEFDP                   NO  - BRANCH            @010C
*
         MVI   PARMDUPS-PARMTBL(R5),C'Y'
*
         BRU   INITPRML                   READ  NEXT  PARM
*
INITEFDP CLC   CARDKEYW(L'EFDP),EFDP  Effective date position     @010I
         JNE   INITPCNT               NO  - BRANCH                @010I
*                                                                 @010I
         llgt  R14,TRTTBLUa       LOAD NUMERIC CLASS TEST TBL ADDR@010I
         CLI   CARDRSRC+3,C' '                                    @010I
         JNE   INITEFD4                                           @010I
         TRT   CARDRSRC(3),0(R14) NUMERIC  ???                    @010I
         JZ    INITEFDL                                           @010I
         J     INITEFDE                                           @010I
*                                                                 @010I
INITEFD4 TRT   CARDRSRC(4),0(R14) NUMERIC  ???                    @010I
         JZ    INITEFDL           YES - CONTINUE                  @010I
*                                                                 @010I
INITEFDE DS    0h                 NON-NUMERIC "EFDP"  MESSAGE
         GVBMSG FORMAT,MSGNO=CK_PARM_NOT_NUM,SUBNO=2,                  +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(EFDP,L'EFDP),                                     +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         J     RTNERROR                                           @010I
*                                                                 @010I
INITEFDL PACK  WKDBLWRK,CARDRSRC(3)                               @010I
         CLI   CARDRSRC+3,C' '                                    @010I
         JE    INITEFDL2                                          @010I
         PACK  WKDBLWRK,CARDRSRC(4)                               @010I
INITEFDL2 DS   0H                                                 @010I
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'                        @010I
         CVB   R0,WKDBLWRK                                        @010I
         BCTR  R0,0                                               @010I
         STH   R0,PARMEFDO-PARMTBL(,R5)                           @010I
*                                                                 @010I
         J     INITPRML                   READ  NEXT  PARM        @010I
*                                                                 @010I
INITPCNT L     R0,WKENTCNT                INCREMENT   ENTITY   COUNT
         AHI   R0,1
         ST    R0,WKENTCNT
*
         if Cij,R0,ne,1,and,              Not first                    +
               CLI,PARMETYP-PARMTBL(R5),eq,C'P' PRECEDING PROF ENTITY
*
           ST  R5,PARMPROA                CONNECT NON-PROFILE TO PROF
         endif
*
         MVC   PARMENT,CARDENT            COPY  ENTITY    ID
         MVC   PARMDDN,BLANKS             COPY  DDNAME    PREFIX
         MVC   PARMDDN(L'CARDDDN),CARDDDN
*
         MVC   PARMRSRC,CARDRSRC          COPY  CATEGORY  PARTITIONS
* RTC19235
* THE following check is not valid as the RXIT 'card' typically follows
*  the entity 'card' with type V
         If (cli,cardetyp,eq,C'V')
           If (CLC,PARMRXIT(8),eq,BLANKS)
            GVBMSG FORMAT,MSGNO=CK_V_ENTITY_ERR,SUBNO=1,               +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
             BRU   RTNERROR
           Else
             MVC   PARMRSRC,BLANKS
             MVC   PARMRSRC(1),CARDRSRC     YES - COPY FIRST CATEGORY
           Endif
         Endif
*
         MVC   PARMETYP,CARDETYP          ENTITY  TYPE
         MVC   PARMCLAP,CARDCLAP          COPY  COLLAPSE  CODE
         MVC   PARMDB2,CARDDB2            COPY  DB2       INDICATOR
         MVC   PARMGDG,CARDGDG            COPY  GDG       INDICATOR
         MVC   PARMDSN,BLANKS             PATTERN DSN
*                                                                 @008I
         MVC   PARMKXIT,BLANKS            ENTITY KEY  EXIT        @008I
         XC    PARMKXEP,PARMKXEP                                  @008I
         MVC   PARMRXIT,BLANKS            ENTITY READ EXIT
         XC    PARMRXEP,PARMRXEP
*                                                                 @008I
         XC    PARMRXCA,PARMRXCA
         MVC   PARMPXIT,BLANKS            PROFILE  ID EXIT
         XC    PARMPXEP,PARMPXEP
         XC    PARMPXCA,PARMPXCA
         XC    PARMOMAP,PARMOMAP
         XC    PARMKMAP,PARMKMAP
*
***********************************************************************
*  EDIT STARTING PARTITION ID                                         *
***********************************************************************
         LAY   R14,TRTTBLU        LOAD  NUMERIC   CLASS TEST  TBL  ADDR
         TRT   CARDPFR,0(R14)     NUMERIC  ???
         BRZ   INITPFRM           YES - CONTINUE
*
*        "BAD PARTITION" MESSAGE
         GVBMSG FORMAT,MSGNO=CK_PART_RANGE_ERR,SUBNO=1,                +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR
*
INITPFRM MVC   PARMPFR,CARDPFR    COPY  STARTING  PARTITION
*
***********************************************************************
*  EDIT ENDING   PARTITION ID                                         *
***********************************************************************
         LAY   R14,TRTTBLU        LOAD  NUMERIC   CLASS TEST  TBL  ADDR
         TRT   CARDPTO,0(R14)     NUMERIC  ???
         BRZ   INITPTOM           YES - CONTINUE
*
*        "BAD PARTITION" MESSAGE
         GVBMSG FORMAT,MSGNO=CK_PART_RANGE_ERR,SUBNO=1,                +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR
*
INITPTOM MVC   PARMPTO,CARDPTO    COPY  ENDING    PARTITION
*
         PACK  WKDBLWRK,CARDPFXL
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         CVB   R0,WKDBLWRK
         if Cij,R0,ge,L'FILEPRFX
           AHI R0,-L'FILEPRFX
         endif
         STH   R0,PARMPFXL
*
***********************************************************************
*  EDIT KEY POSITION                                                  *
***********************************************************************
         if CLI,CARDKEYP,eq,C' '  LEADING SPACE  ???
           MVI CARDKEYP,C'0'
         endif
*
         LAY   R14,TRTTBLU        LOAD  NUMERIC  CLASS TEST  TBL  ADDR
         TRT   CARDKEYP,0(R14)    NUMERIC  ???
         BRZ   INITKYPM           YES - CONTINUE
*
*        "BAD KEY POSITION" MESSAGE
         GVBMSG FORMAT,MSGNO=CK_KEY_POS_ERR,SUBNO=1,                   +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR
*
INITKYPM PACK  WKDBLWRK,CARDKEYP
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         CVB   R0,WKDBLWRK
         BCTR  R0,0
         STH   R0,PARMKEYO
*
***********************************************************************
*  EDIT KEY LENGTH                                                    *
***********************************************************************
         if CLI,CARDKEYL,eq,C' '  LEADING SPACE  ???
           MVI CARDKEYL,C'0'
         endif
*
         LAY   R14,TRTTBLU        LOAD  NUMERIC  CLASS TEST  TBL  ADDR
         TRT   CARDKEYL,0(R14)    NUMERIC  ???
         BRZ   INITKYLM           YES - CONTINUE
*
*        DISPLAY "BAD KEY LENGTH"   MESSAGE
         GVBMSG FORMAT,MSGNO=CK_KEY_LEN_ERR,SUBNO=1,                   +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR
*
INITKYLM PACK  WKDBLWRK,CARDKEYL
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         CVB   R0,WKDBLWRK
         STH   R0,PARMKEYL
*
         l     r8,wkkeyln                  RTC19235
         if   (cij,r8,eq,0)
           sth   R0,wkkeyln              store lowest key length
         else                            to check if key length lt
           if (ch,r0,lt,wkkeyln)         break length
             sth R0,wkkeyln
           endif
         endif
*
         LTR   R0,R0
         BRNP  INITPTMS
         CH    R0,WKMAXKEY
         BRNH  INITPTMS
         STH   R0,WKMAXKEY
*
INITPTMS PACK  WKDBLWRK,CARDTMSP
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         CVB   R0,WKDBLWRK
         BCTR  R0,0
         STH   R0,PARMTMSO
*
         PACK  WKDBLWRK,CARDEFDP
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         CVB   R0,WKDBLWRK
         BCTR  R0,0
         STH   R0,PARMEFDO
*
         PACK  WKDBLWRK,CARDPROP
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         CVB   R0,WKDBLWRK
         BCTR  R0,0
         STH   R0,PARMPROO
*
         LHI   R0,L'PARMRSRC
         LA    R14,PARMRSRC+L'PARMRSRC
         xgr   R15,R15            ZERO CATEGORY COUNT
*
INITPFIL BCTR  R14,0
         if CLI,0(R14),ne,C' '
           AHI R15,1
         endif
         BRCT  R0,INITPFIL
*
         if ltgr,R15,R15,np
           lghi R15,1
         endif
*
         agf   R15,WKFILCNT
         ST    R15,WKFILCNT
*
* RTC19235 check that blank entity does not follow T type entity
*
         if  (cli,wktypsw,eq,c'T'),and,(cli,cardetyp,eq,c' ')
*          "Blank Entity type follows T"
           GVBMSG FORMAT,MSGNO=CK_T_ENTITY_ERR,SUBNO=1,                +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
            BRU   RTNERROR
         endif
         mvc   wktypsw,cardetyp   save the entity type
*
INITPNXT ds    0H
*
         LGR   R5,R7              SAVE PREVIOUS RECORD ADDRESS
*
         LA    R7,PARMLEN(,R7)
         BRCT  R3,INITPRML
*
         llgt  R2,WKPRMDCB        LOAD PARM FILE DCB ADDRESS
         MVC   REENTWK(8),OPENPARM
         CLOSE ((R2)),MODE=31,MF=(E,REENTWK)   CLOSE FILE
*
* MAXIMUM parameter lines exceeded
*
         LHI   R15,MAXPARM
         CVD   R15,WKDBLWRK
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         UNPK  WKDBLWK2(2),WKDBLWRK
         GVBMSG FORMAT,MSGNO=CK_MERGPRM_EXCEEDED,SUBNO=2,              +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(WKDBLWK2,2),                                      +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
         BRU   RTNERROR
*
initpeof ds    0h
         llgt  R2,WKPRMDCB        LOAD PARM FILE DCB ADDRESS
         MVC   REENTWK(8),OPENPARM
         CLOSE ((R2)),MODE=31,MF=(E,REENTWK)     CLOSE     FILE
*
         if   (clc,wkbrklen,ne,XHEXFF)          RTC19235
           if   (clc,wkkeyln,lt,wkbrklen)
*  "Key length < Break Length"
             GVBMSG FORMAT,MSGNO=CK_BREAKLEN_ERR1,SUBNO=1,             +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
             BRU   RTNERROR
           endif
         endif
*
         L     R0,WKENTCNT        SAVE ENTITY    COUNT
         STH   R0,GLOBECNT
         L     R0,WKFILCNT        SAVE FILE AREA COUNT
         STH   R0,GLOBFCNT
         LH    R0,WKBRKLEN        SAVE BREAK     LENGTH
         STH   R0,GLOBBRKL
         LH    R0,WKMAXKEY        SAVE MAX  KEY  LENGTH
         STH   R0,GLOBKEYM
*
         if  lt,r0,globbsiz,z     if no "BUFSIZE" parm then use
           ly r0,lrbsize            "LRBUFFER" parm plus
           ay r0,lrxsize            "EXTENSION" parm
           st r0,globbsiz
         endif
         if  lt,r0,globtsiz,z     if no "TBUFSIZE" parm then
           asi globtsiz,1           default to 1MB
         endif
*
         DROP  R11
         DROP  R7
                        EJECT
***********************************************************************
*  LOAD  RECORD INITIALIZATION AREA                                   *
***********************************************************************
LOADINI  LHI   R0,INIFILEL        LOAD DCB  LENGTH
         GETMAIN RU,LV=(0),LOC=(BELOW) GET  MEMORY FOR DCB
         ST    R1,WKINIDCB        SAVE DCB ADDRESS IN  CONTROL ELEMENT
         lgr   R2,R1
         USING IHADCB,R2
*
         LAY   R14,INIFILE             COPY  DCB
         MVC   0(INIFILEL,R2),0(R14)
*
         LAY   R14,INIDCBE-INIFILE(,R2)
         ST    R14,DCBDCBE
*
         llgt  R14,GPSTARTA       LOAD  START-UP  DATA   ADDRESS
         USING STRTDATA,R14
         if CLC,SDDDNPFX,ne,BLANKS  CHECK IF DDNAME PREFIX SPECIFIED
           MVC DCBDDNAM(L'SDDDNPFX),SDDDNPFX
         endif
         DROP  R14
*
         MVC   REENTWK(8),OPENPARM
         OPEN  ((R2),INPUT),MODE=31,MF=(E,REENTWK)  OPEN FILE
         TM    48(R2),X'10'                         SUCCESSFUL ???
         BRNO  INITGLCR
*
         L     R0,WKENTCNT        LOAD  MAX NUM OF ENTITIES (MAXIMUM+1)
         AHI   R0,1
         LA    R1,INITLEN         LOAD  MAXIMUM INITIALIZATION RECORD
         AH    R1,DCBLRECL
         MR    R0,R0              COMPUTE AREA SIZE
         lgr   R2,R1              SAVE    AREA SIZE
*
         lgr   R0,R1
         GETMAIN RU,LV=(0),LOC=(ANY)
         ST    R1,GLOBRINB        SAVE  RECORD INITIALIZATION - BEG
         ST    R1,WKRINITB
         ST    R1,GLOBRINE
         ST    R1,WKRINITE
         agr   R1,R2
         ST    R1,GLOBRINM        SAVE  RECORD INITIALIZATION - END
         ST    R1,WKRINITM
*
         llgt  R0,WKRINITB        INITIALIZE   AREA TO X'FF'
         lgr   R1,R2
         sgr   R14,R14
         sgr   R15,R15
         ICM   R15,B'1000',XHEXFF
         MVCL  R0,R14
*
         llgt  R2,WKINIDCB        LOAD PARM FILE DCB ADDRESS
*
         LGF   R3,WKENTCNT        LOAD MAXIMUM  NUMBER OF    ENTRIES
         llgt  R7,WKRINITB        LOAD FIRST INITIALIZATION  REC ADDR
         USING RECINIT,R7
*
***********************************************************************
*  READ INITIALIZATION RECORDS INTO MEMORY                            *
***********************************************************************
INITINIL ds    0h
         GET   (R2)               READ NEXT INITIALIZATION  RECORD
*
         MVC   INITENT,0(R1)           COPY ENTITY  ID  TO  TABLE
*
         PACK  WKDBLWRK,L'INITENT(4,R1)     CONVERT RECORD  LENGTH
         CVB   R15,WKDBLWRK
         LA    R0,4(,R15)         ADD "RDW" LENGTH
         STH   R0,INITRLEN
         XC    INITFLAG,INITFLAG
*
         LA    R14,L'INITENT+4(,R1)    COPY INITIALIZATION  DATA
         LA    R0,INITDATA
         lgr   R1,R15
         MVCL  R0,R14
*
         LGR   R7,R0
         LGF   R15,WKRINITM       ROOM  FOR ANOTHER  RECORD ???
         SGR   R15,R7
         LGHI  R0,INITLEN
         SGR   R15,R0
         CH    R15,DCBLRECL
         BRNL  INITINIL
*
         MVC   REENTWK(8),OPENPARM
         CLOSE ((R2)),MODE=31,MF=(E,REENTWK)    CLOSE  FILE
*
*        "INITIALIZATION TBL OVERFLOW"
         GVBMSG FORMAT,MSGNO=CK_REC_INIT_TABLE_OVERFLOW,SUBNO=2,       +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(DCBDDNAM,L'DCBDDNAM),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR
*
INITIEOF ds    0h
         ST    R7,GLOBRINE
         ST    R7,WKRINITE
*
         MVC   REENTWK(8),OPENPARM
         CLOSE ((R2)),MODE=31,MF=(E,REENTWK)    CLOSE  FILE
*
         DROP  R7
*
***********************************************************************
*  CREATE GLOBAL NAME/TOKEN AREA                                      *
***********************************************************************
INITGLCR CALL  IEANTCR,(TOKNLVL2,WKTOKNAM,WKTOKN,TOKNPERS,WKTOKNRC),   +
               MF=(E,REENTWK)
*
         Ltgf  R15,WKTOKNRC       SUCCESSFUL  ???
         BRZ   INITTOKN
*
         CVD   R15,WKDBLWRK
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         UNPK  WKDBLWK2(3),WKDBLWRK
*
         GVBMSG FORMAT,MSGNO=CREATE_TOKEN_FAIL,SUBNO=3,                +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(WKTOKNAM,16),                                     +
               SUB3=(WKDBLWK2,3),                                      +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR
*
***********************************************************************
*  CREATE NAME/TOKEN FOR PASSING SHARED THREAD AREAS TO "GVBXLCK"     *
***********************************************************************
INITTOKN MVC   WKTOKNAM+0(8),GENEVA
         MVC   WKTOKNAM+8(8),WKDDNAME
*
         XC    WKTOKN,WKTOKN
*   switching to AMODE64 now
         sysstate amode64=YES
         sam64
*
         llgt  r15,=v(iean4rt)    get address without bit 32
         CALL  (15),(TOKNLVL1,WKTOKNAM,WKTOKN,WKTOKNRC),               X
               MF=(E,REENTWK)
         ltgf  R15,WKTOKNRC       SUCCESSFUL   ???
         BRNZ  INITPART           NO  - CREATE THE FIRST ONE
*
         llgt  R1,WKTOKNBA
         STG   R1,WKBUFBEG
         STG   R1,WKBUFEND
*
         LGH   R0,GLOBBSIZ       "LR BUFFER" SIZE SPECIFIED ???
         SLLG  R0,r0,20           CONVERT TO MEGABYTES
         if LTGR,R0,R0,np         If result is not positive
           LGFi R0,F25MEG          DEFAULT   "LR BUFFER"   SIZE
         endif
*
         AGR   R1,R0
         STG   R1,WKBUFMAX
*
         llgt  R1,WKTOKNDA
         ST    R1,WKDIRBEG
*
         llgt  R1,WKTOKNPA        LOAD PREVIOUS PARTITION STAT AREA
         USING PARTAREA,R1
*
         llgt  R14,PARTLSTB
         llgt  R0,0(,R14)
         ST    R0,WKLSTBEG
         ST    R0,WKLSTCUR
         ST    R0,WKLSTEND
         agf   R0,WKLSTSIZ                                        @020C
         ST    R0,WKLSTMAX
*
         LA    R14,PROTBLEN       COMPUTE TABLE SIZE
         LHI   R15,MAXPROID
         MR    R14,R14
         ST    R15,WKPROSIZ
*
         llgt  R0,PARTPROB
         ST    R0,WKPROBEG
         ST    R0,WKPROEND
         agf   R0,WKPROSIZ
         ST    R0,WKPROMAX
*
         xc    PARTFNDE,PARTFNDE
         xc    PARTFNDT,PARTFNDT
         xc    PARTFNDX,PARTFNDX
         xc    PARTFNDR,PARTFNDR
         xc    PARTFNDB,PARTFNDB
         xc    PARTNOTE,PARTNOTE
         xc    PARTNOTT,PARTNOTT
         xc    PARTNOTX,PARTNOTX
         xc    PARTNOTR,PARTNOTR
         xc    PARTNOTB,PARTNOTB
         xc    PARTBRKC,PARTBRKC
         xc    PARTMDSW,PARTMDSW
*
*
***********************************************************************
*  INITIALIZE SHARED POINTERS FOR "LR BUFFER", TRANSACTION, GENERATED *
***********************************************************************
*
         DROP  R1
*
         BRU   INITDEQ
*
*
***********************************************************************
*  Create token for possible other exits call later                   *
***********************************************************************
*
initpart ds    0h
***********************************************************************
*  ALLOCATE "BUFSIZE" and "TBUFSIZE" areas                            *
***********************************************************************
         llgt  R15,WKMR95WA                     THREAD WORK AREA
         using thrdarea,r15
         if CLI,THREAD_MODE,eq,C'S'
           ST R10,USERTOKN+0                    YES - LEFT HALF NON-00
         endif
         ST    R13,USERTOKN+4                   RIGHT HALF NON-ZERO
*
         drop  r15
         lgf   r1,globbsiz           Get "BUSIZE"
         agf   r1,globtsiz           add on transaction buffer size
         stg   r1,wkdblwrk           save for IARV64

         IARV64 REQUEST=GETSTOR,SEGMENTS=wkdblwrk,ORIGIN=wkbufbeg, pgc X
               FPROT=NO,SVCDUMPRGN=YES,                                X
               TTOKEN=TCBTOKEN,USERTKN=USERTOKN,                       X
               MF=(E,GET64MEM,COMPLETE)
*
*
*        "BUFSIZE"
*
         lg    r1,wkbufbeg       get start of buffer
         mvc   0(l'xrckbufr,r1),xrckbufr  move in eyecatcher
         aghi  r1,l'xrckbufr     real start of buffer
         lgf   r15,globbsiz       Get original "BUFSIZE"
         sllg  r15,r15,20          remember size in MB
         lghi  r0,l'xrckbufr     allow for eyecatcher
         sgr   r15,r0             allow for eyecatcher
         stg   r1,wkbufbeg
         stg   r1,wkblkbdw
         stg   r1,wkbufend
         agr   r1,r15
         stg   r1,wkbufmax
*
*
*        Generated transactions will be written into the buffer after
*        the last block of the key break records and will use the rest
*        of the buffer. We will default to this but they will be
*        changed later.
*
*
         stg   r1,wkgenbeg
         stg   r1,wkgenbdw
         stg   r1,wkgenend
         stg   r1,wkgenmax
*
*
*        "TBUFSIZE"
*
         mvc   0(l'xrcktran,r1),xrcktran  move in eyecatcher
         aghi  r1,l'xrcktran     real start of buffer
         stg   r1,wktrnbdw       Start of the transaction buffer
         la    R0,4(,r1)            default end of block,only RDW
         stg   r0,wktrnend          save it
         lgf   r15,globtsiz      Get original "TBUFSIZE"
         sllg  r15,r15,20        remember size in MB
         lghi  r0,l'xrcktran     allow for eyecatcher
         sgr   r15,r0            allow for eyecatcher
         agr   r15,r1            Get end point of transaction buffer
         stg   r15,wktrnmax         and save it
*
*
***********************************************************************
*  COMPUTE SIZE OF SHARED DIRECTORY  AND  ALLOCATE MEMORY             *
***********************************************************************
         LHI   R0,DIRLEN          ALLOCATE  "SHARED   DIRECTORY"
         L     R1,WKENTCNT        MAXIMUM NO OF ENTITIES
         AHI   R1,1               ADD +1 FOR GENERATED ENTITIES
         MR    R0,R0
         lgr   R0,R1
         GETMAIN RU,LV=(0),LOC=(ANY),BNDRY=PAGE
         ST    R1,WKDIRBEG
*
*
***********************************************************************
*  ALLOCATE "RECORD LIST" - POINTERS TO RECORDS IN "LR BUFFER"        *
***********************************************************************
         lgf   R0,WKLSTSIZ      ALLOCATE "RECORD LIST" (DBLWORDS) @020C
         GETMAIN RU,LV=(0),LOC=(ANY),BNDRY=PAGE
         ST    R1,WKLSTBEG
         ST    R1,WKLSTCUR
         ST    R1,WKLSTEND
         agf   R1,WKLSTSIZ                                        @020C
         ST    R1,WKLSTMAX
*
*
***********************************************************************
*  COMPUTE COMBINED SIZE OF FILE CONTROL AREAS  AND  ALLOCATE MEMORY  *
***********************************************************************
         LA    R0,FILELEN         LOAD  LENGTH OF EACH CONTROL BLOCK
         L     R1,WKFILCNT        LOAD  NUMBER OF FILE CONTROL BLOCKS
         MR    R0,R0
         AHI   R1,8               END-OF-TABLE MARKER  LENGTH
         ST    R1,WKFILSIZ
         AHI   R1,8               EYEBALL
*
         lgr   R0,R1
         GETMAIN RU,LV=(0),LOC=(ANY),BNDRY=PAGE
*
         MVC   0(8,R1),XRCKFILE
         AHI   R1,8
         ST    R1,WKFILBEG        SAVE  FILE   CONTROL BLOCK   ADDRESS
         agf   R1,WKFILSIZ
         aghi  R1,-8              END-OF-TABLE MARKER  LENGTH
         ST    R1,WKFILMAX
         MVC   0(8,R1),XHEXFF
*
*
***********************************************************************
*  ALLOCATE "PROFILE ID ASSIGNMENT TABLE"                             *
***********************************************************************
         LA    R0,PROTBLEN        COMPUTE TABLE SIZE
         LHI   R1,MAXPROID
         MR    R0,R0
         ST    R1,WKPROSIZ
*
         lgr   R0,R1
         GETMAIN RU,LV=(0),LOC=(ANY)
         ST    R1,WKPROBEG
         ST    R1,WKPROEND
         agf   R1,WKPROSIZ
         ST    R1,WKPROMAX
*
*
***********************************************************************
*  INITIALIZE PARTITION DDNAME TOKEN FIELDS                           *
***********************************************************************
         MVC   WKTOKNBA,WKBUFBEG+4
         MVC   WKTOKNDA,WKDIRBEG
         MVC   WKTOKNFA,WKFILBEG
*
*
***********************************************************************
*  ALLOCATE SHARED PARTITION DDNAME WORK AREA                         *
***********************************************************************
         lghi  R0,PARTLEN+8
         GETMAIN RU,LV=(0),LOC=(ANY)
         MVC   0(8,R1),PARTEYEB
         aghi  R1,8
         ST    R1,WKTOKNPA
         USING PARTAREA,R1
*
         XC    0(PARTLEN,R1),0(R1)  ZERO "GVBXLCK" PARTITION AREA
*
         ST    R13,PARTWORK         SAVE WORK   AREA ADDRESS
         MVC   PARTGLOB,WKGLOBA     SAVE GLOBAL AREA ADDRESS
         MVC   PARTDIRA,WKDIRBEG    SAVE DIRECTORY   ADDRESS
         MVC   PARTFILA,WKFILBEG    SAVE FILE AREA   ADDRESS
*
         xc    PARTFNDE,PARTFNDE
         xc    PARTFNDT,PARTFNDT
         xc    PARTFNDX,PARTFNDX
         xc    PARTFNDR,PARTFNDR
         xc    PARTFNDB,PARTFNDB
         xc    PARTNOTE,PARTNOTE
         xc    PARTNOTT,PARTNOTT
         xc    PARTNOTX,PARTNOTX
         xc    PARTNOTR,PARTNOTR
         xc    PARTNOTB,PARTNOTB
         xc    PARTBRKC,PARTBRKC
         xc    PARTMDSW,PARTMDSW
*
         LA    R0,WKLSTBEG
         ST    R0,PARTLSTB
         MVC   PARTPROB,WKPROBEG
*
         larl  R0,GENCKTRN
         ST    R0,CKTRNADR
         larl  R0,GENCKVRT
         ST    R0,CKVRTADR
         larl  R0,GENCKSB
         ST    R0,CKSBADR
         larl  R0,GENCKBR
         ST    R0,CKBRADR
         larl  R0,GENCKTKN
         ST    R0,CKTKNADR
         larl  R0,GENCKDIR
         ST    R0,CKDIRADR
*
*
***********************************************************************
*  INITIALIZE SHARED POINTERS FOR "LR BUFFER", TRANSACTION, GENERATED *
***********************************************************************
*
         DROP  R1
*
         llgt  R1,WKMR95WA
         USING THRDAREA,R1
         LA    R15,WKTOKNAM
         ST    R15,IMSWADDR
         DROP  R1
         llgt  r15,=v(iean4cr)    get address without bit 32
         CALL  (15),(TOKNLVL1,WKTOKNAM,WKTOKN,TOKNPERS,WKTOKNRC),      +
               MF=(E,REENTWK)
*
         ltgf  R15,WKTOKNRC       SUCCESSFUL  ???
         BRZ   INITDEQ
*
         CVD   R15,WKDBLWRK
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         UNPK  WKDBLWK2(3),WKDBLWRK
*
         GVBMSG FORMAT,MSGNO=CREATE_TOKEN_FAIL,SUBNO=3,                +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(WKTOKNAM,16),                                     +
               SUB3=(WKDBLWK2,3),                                      +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR
*
INITDEQ  MVC   REENTWK(MDLDEQPL),MDLDEQP
         DEQ   MF=(E,REENTWK)
*
         DROP  R4
                        EJECT
***********************************************************************
*  COMPUTE SIZE OF OPEN FILE LIST  AND  ALLOCATE MEMORY               *
***********************************************************************
         lgf   R1,WKFILCNT        LOAD  NUMBER OF FILES
         LA    R1,1(,R1)          ADD   ONE   FOR TERMINATOR
         ST    R1,WKOPNCNT        SAVE  ADJUSTED  COUNT
         SLLg  R1,r1,2            MULTIPLY BY  4
         ST    R1,WKOPNSIZ        SAVE  TOTAL SIZE
         lgr   R0,R1              ALLOCATE    OPEN FILE LIST
         GETMAIN RU,LV=(0),LOC=(ANY)
*
         ST    R1,WKOPNBEG        SAVE  BEGINNING  ADDRESS
         agf   R1,WKOPNSIZ
         ST    R1,WKOPNMAX        SAVE  ENDING     ADDRESS
*
         llgt  R0,WKOPNBEG        INITIALIZE  LIST TO X'FF'
         lgf   R1,WKOPNSIZ
         sgr   R14,R14
         IC    R15,XHEXFF
         SLL   R15,24
         MVCL  R0,R14
                        EJECT
***********************************************************************
*  INITIALIZE FILE CONTROL AREAS                                      *
***********************************************************************
INITFC   llgt  R8,WKOPNBEG           LOAD FIRST OPEN    FILE ENTRY ADDR
         llgt  R6,WKFILBEG           LOAD FIRST FILE CONTROL BLOCK ADDR
         USING FILEAREA,R6
*
         LA    R7,FILERTYP           INIT PREVIOUS FILE TYPE BREAK
*
         LGF   R2,WKENTCNT
         llgt  R3,WKPARMA
         USING PARMTBL,R3
*
INITFILE LA    R4,L'PARMRSRC
         LA    R5,PARMRSRC
*
         CLC   PARMRSRC,BLANKS       ANY     CATEGORIES PRESENT ???
         BRNE  INITFSRC              YES -   USE  THEM
         LGHI  R4,1                  NO  -   JUST USE   ONE  BLANK ONE
         BRU   INITFINI
*
INITFSRC CLI   0(R5),C' '            CATEGORY  PRESENT  ???
         BRE   INITSNXT              NO  -   TRY  NEXT  POSITION
*
INITFINI MVC   FILERTYP,PARMENT      EVENT   RECORD TYPE
         MVC   FILERSRC,0(R5)        EVENT   RECORD SOURCE
         MVC   FILEPART,BLANKS       PARTITION
         MVC   FILEETYP,PARMETYP     ENTITY  TYPE
         MVC   FILECLAP,PARMCLAP     ENTITY  TYPE
         MVI   FILEEOF,C' '          EVENT  "END-OF-FILE"   INDICATOR
         MVC   FILENEWB,PARMNEWB     ENTITY  BEGINS NEW LR  BLOCK
         XC    FILERECL,FILERECL     EVENT   RECORD LENGTH (CURRENT REC
         XC    FILEMAXL,FILEMAXL     EVENT   RECORD MAXIMUM  LENGTH
         xc    FILEICNT,fileicnt     EVENT   RECORD COUNT  - INPUT
         xc    FILEDCNT,filedcnt     EVENT   RECORD COUNT  - DUPLICATE
*
*
         LH    R14,PARMPFXL          EMBEDDED  PARTITION LENGTH
         STH   R14,FILEPRTL
*
         LH    R14,PARMKEYO          EVENT   KEY    OFFSET
         STH   R14,FILEKEYO
*
         LH    R14,PARMKEYL          EVENT   KEY    LENGTH (CURRENT REC
         STH   R14,FILEKEYL
*
         LH    R14,WKBRKLEN
         STH   R14,PARMBRKL
*
         STH   R14,FILEBRKL
*
         LH    R14,PARMTMSO          TIMESTAMP      OFFSET
         STH   R14,FILETMSO
*
         LH    R14,PARMEFDO          EFFECTIVE DATE OFFSET
         STH   R14,FILEEFDO
*
         LH    R14,PARMPROO          PROFILE ID     OFFSET
         STH   R14,FILEPROO
*
         XC    FILEOPNA,FILEOPNA     OPEN    FILE   LIST     ENTRY  ADR
         XC    FILEPRMA,FILEPRMA     PARAMETER      AREA     ADDRESS
         XC    FILEPROA,FILEPROA     PROFILE PARM   ENTRY    ADDRESS
         XC    FILEINIA,FILEINIA     INITIALIZATION RECORD   ADDRESS
         XC    FILEMINA,FILEMINA     DIRECTORY      ENTRY    ADDRESS
*
         if CLI,FILEETYP,ne,C'T'     NON-BUFFERED  ENTITY ???
           llgt R14,WKDIROFF         SHARED  DIRECTORY - 1ST RECORD ADR
           agf R14,WKDIRBEG
         endif
         ST    R14,FILEMINA
*
         XC    FILEBLKA,FILEBLKA     EVENT   BLOCK  ADDRESS(CURRENT REC
         XC    FILEEODA,FILEEODA     EVENT   BLOCK  END-OF-DATA ADDRESS
         XC    FILERECA,FILERECA     EVENT   RECORD ADDRESS(CURRENT REC
*
         XC    FILEVREC,FILEVREC     DECOMPRESSED VIEW DEFN REC ADDRESS
*
         XC    FILEKXEP,FILEKXEP     ENTITY KEY EXIT TRUE ENTPT   @008I
         XC    FILERXEP,FILERXEP     ENTITY  READ EXIT TRUE ENTRY POINT
*                                                                 @008I
         XC    FILERXCA,FILERXCA     ENTITY  READ EXIT CALL ADDRESS
         XC    FILERXAN,FILERXAN     ENTITY  READ EXIT ANCHOR
*
         XC    FILEPXEP,FILEPXEP     PROFILE   ID EXIT TRUE ENTRY POINT
         XC    FILEPXCA,FILEPXCA     PROFILE   ID EXIT CALL ADDRESS
         XC    FILEPXAN,FILEPXAN     PROFILE   ID EXIT ANCHOR
*
         XC    UR20RC,UR20RC         RETURN   CODE
         XC    UR20ERRC,UR20ERRC     ERROR    CODE
         XC    UR20RECL,UR20RECL     RECORD   LENGTH
         XC    UR20RECA,UR20RECA     RECORD   AREA      ADDRESS
         XC    UR20RBN,UR20RBN       RELATIVE BLOCK     NUMBER
         MVC   UR20DDN,PARMDDN       DDNAME
*
         lghi  R0,L'WKDDNAME
         LA    R14,WKDDNAME+L'WKDDNAME
INITFPRT bctgr R14,0
         if CLI,0(R14),eq,C' '
           BRCT R0,INITFPRT
           BRU INITFDD
         endif
*
         aghi  R14,-1
         CLI   0(R14),C'0'
         BRL   INITFDDX
         CLI   0(R14),C'9'
         BRH   INITFDDX
         CLI   1(R14),C'0'
         BRL   INITFDDX
         CLI   1(R14),C'9'
         BRH   INITFDDX
         MVC   FILEPART,0(R14)
         J     INITFDD
*
INITFDDX DS    0H
*       Invalid DDNAME - PF ddname must end with 2 digits
*
         L     R0,WKLFID
         CVD   R0,WKDBLWRK
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         UNPK  WKDBLWK2(8),WKDBLWRK
         GVBMSG FORMAT,MSGNO=CK_PF_DDNAME_ERR,SUBNO=2,                 +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(WKDBLWK2,L'WKDBLWK2),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR

*
INITFDD  lghi  R0,L'UR20DDN
         LA    R14,UR20DDN+L'UR20DDN
INITFDDN bctgr R14,0
         if CLI,0(R14),eq,C'%'
           MVC 0(1,R14),FILERSRC
         endif
*
         if CLI,0(R14),eq,C'!',and,                                    +
               cgij,R0,ne,1
*
           bctgr R14,0
           bctgr R0,0
           if CLI,0(R14),eq,C'!'
             MVC 0(2,R14),FILEPART
           endif
         endif
*
         BRCT  R0,INITFDDN
*                                    RTC19912 eliminate EXCP I/O
         MVI   UR20OPT1,C'I'         I/O MODE(I=INPUT,O=OUTPUT,D=DIREC
*
INITFOPT MVI   UR20OPT2,C' '
*
*        The BUFNO value passed from here is an override.
*        If the value is 0 then the IO routine will used MULTSDN
*
         ltgf  r0,wkbufno
*        if (ltgf,r0,wkbufno,np)
*          LHI   R0,FILENBUF         NUMBER OF I/O    BUFFERS
*        endif
         STH   R0,UR20NBUF
*
         XC    UR20WPTR,UR20WPTR     WORK   AREA      POINTER
         XC    UR20ADDR,UR20ADDR     READ   ROUTINE   ADDRESS
*
         LA    R0,UR20FC             OPEN   PARAMETER LIST
         ST    R0,UR20PARM
         OI    UR20PARM,X'80'
*
         ST    R3,FILEPRMA           SAVE   PARAMETER AREA ADDRESS
*
         if (ltgf,r0,parmproa,p)
           ST    R0,FILEPROA
         endif
*
         MVC   FILEKXEP,PARMKXEP     KEY   EXIT TRUE ENTRY POINT  @008I
         MVC   FILERXEP,PARMRXEP     READ  EXIT TRUE ENTRY POINT
*                                                                 @008I
         MVC   FILERXCA,PARMRXCA     READ  EXIT CALL ADDRESS
*
         MVC   FILEPXEP,PARMPXEP     PROFILE ID TRUE ENTRY POINT
         MVC   FILEPXCA,PARMPXCA     PROFILE ID CALL ADDRESS
*
         LGR   R15,R5                COMPUTE   CATEGORY  OFFSET
         LA    R0,PARMRSRC
         SR    R15,R0
*
         LA    R14,PARMOMAP(R15)
         MVC   FILEOMAP,0(R14)       COPY OUTPUT   FILE  MAPPING
         LA    R14,PARMKMAP(R15)
         MVC   FILEKMAP,0(R14)       COPY LAST KEY VALUE MAPPING
*
         if CLI,FILEOMAP,eq,C' '         MAPPED TO BLANK ???
           MVC FILEOMAP,FILERSRC
         endif
         if CLI,FILEKMAP,eq,C' '         MAPPED TO BLANK ???
           MVC FILEKMAP,FILERSRC
         endif
*
***********************************************************************
*  CHECK IF FILE WAS INCLUDED IN PARTITION RANGE                      *
***********************************************************************
         CLC   FILEPART,PARMPFR      CHECK  PARTITION RANGE
         BRL   INITEOF               INITEOF01
         CLC   FILEPART,PARMPTO      CHECK  PARTITION RANGE
         BRH   INITEOF               INITEOF01
*                                                                 @008I
*                                                                 @008I
******************************************************************@008I
*  ISSUE OPEN PHASE CALL TO EXITS                                 @008I
******************************************************************@008I
         LT    R0,FILERXEP        CALL  "READ EXIT" IF  SPECIFIED @008I
         BRNZ  INITCALL                                           @008I
*                                                                 @008I
         LT    R0,FILEKXEP        CHECK "KEY  EXIT" IF  SPECIFIED @008I
         BRZ   INITVIRT                                           @008I
*                                                                 @008I
INITCALL llgt  R14,FILEPRMA       RELATED  PARMTBL ENTRY          @008I
         LA    R1,PARMRXPM-PARMTBL(,R14)                          @008I
         ST    R1,WKCALLP3                                        @008I
*                                                                 @008I
         LA    R1,FILERXAN        WORK AREA ANCHOR                @008I
         ST    R1,WKCALLP7                                        @008I
         LA    R1,FILERXRC        RETURN  CODE                    @008I
         ST    R1,WKCALLP8                                        @008I
*                                                                 @008I
         XC    WKCALL64,WKCALL64  Null 64-bit Event Record address
         LAY   R1,WKCALL64        Load pointer to the address
         STY   R1,WKCALLP4        Store pointer in parm list
         XC    WKCALLPT,WKCALLPT  INITIALIZE RETURN RECORD ADDRESS@008I
*                                                                 @008I
         LH    R1,FILERECL        INITIALIZE RECORD LENGTH        @008I
         ST    R1,WKCALLRL                                        @008I
*                                                                 @008I
         XC    FILERXRC,FILERXRC  ZERO RETURN CODE                @008I
*                                                                 @008I
         LA    R1,WKCALLPL                                        @008I
         llgf  R15,FILERXCA       (R0: TRUE ENTRY POINT)          @008I
         bassm R14,R15            no mode switch                  @008I
*                                                                 @008I
         L     R15,FILERXRC       PROCESS RECORD ??               @008I
         cije r15,16,rtnabend     ABORT   ???
*
*
***********************************************************************
*  CHECK IF FILE IS A VIRTUAL FILE                                    *
***********************************************************************
INITVIRT DS    0H                                                 @008I
         CLI   FILEETYP,C'V'         VIRTUAL FILE ???
         BRNE  INITRINI              NO  - BYPASS EXIT  CHECK
*
         OC    FILERXEP,FILERXEP     READ  EXIT SPECIFIED ???
         BRZ   INITEOF02             NO  - MARK AS  EMPTY
         MVI   FILEEOF,C'Y'
         BRU   OPENBRK               YES - BYPASS PHYSICAL FILE OPEN
*
*
***********************************************************************
*  FIND MATCHING INITIALIZATION RECORD                                *
***********************************************************************
INITRINI llgt  R14,WKRINITB          RECORD INITIALIZATION RECORD  ADDR
         USING RECINIT,R14
*
INITRINL cgf   R14,WKRINITE
         BRNL  INITRINF
*
         CLC   INITENT,FILERTYP
         BRNE  INITRINN
*
         LH    R0,INITRLEN
         AHI   R0,-4
         STH   R0,FILEMAXL
*
         LA    R0,INITDATA
         ST    R0,FILEINIA
         BRU   INITUR20
*
INITRINN lgh   R0,INITRLEN
         agr   r14,r0
         LA    R14,L'INITENT(,R14)
         BRU   INITRINL
*
INITRINF MVC   FILEMAXL,XHEXFF       INDICATE INIT REC NOT FOUND
         MVC   FILEINIA,XHEXFF
*
         DROP  R14
*
INITUR20 MVC   UR20ADDR,GVBUR20      ASSUME NOT  DB2
         CLI   PARMDB2,C'Y'
         BRNE  INITDYNA
*
INITDB2  MVC   UR20ADDR,GVBUR30
         MVC   UR20DDN+0(3),DB2
*
*
***********************************************************************
*  DYNAMICALLY ALLOCATE UNALLOCATED DDNAMES  (R1 ==> PARM TBL ENTRY)  *
***********************************************************************
INITDYNA larl  R15,DYNALLOC          DYNAMICALLY ALLOCATE FILES
         oilh  r15,x'8000'           make it amode 31
         bassm R14,R15
*
*
***********************************************************************
*  OPEN FILE AND SAVE ATTRIBUTES                                      *
***********************************************************************
         MVC   UR20FC,H000           FUNCTION CODE  -  OPEN
         xc    ur20mems,ur20mems     init to zero
         LA    R1,UR20PARM
         llgf  R15,UR20ADDR
*
         if clgf,R15,eq,GVBUR30
           MVC UR20WPTR,WKUR20WA
         endif
*
         Bassm R14,R15
*
         llgf  R14,UR20ADDR          SAVE RETURNED DB2 WORKAREA ADDR

         if clgf,R14,eq,GVBUR30
           MVC WKUR20WA,UR20WPTR
         endif
*
         lgh   R15,UR20RC            SUCCESSFUL  OPEN (RC + EC) ???
         LTR   R15,R15
         BRNZ  INITFAIL
*
***********************************************************************
*  SAVE MAXIMUM RECORD LENGTH FOR THIS ENTITY TYPE (STRUCTURE ID)     *
*  AT ENTITY TYPE BREAK: COPY MAXIMUM  LENGTH INTO  FILE AREAS        *
***********************************************************************
         MVC   FILERECF,UR20OPT2     SAVE    RECORD    FORMAT
*
         LH    R0,UR20RECL           RECORD  LENGTH
         if CLI,FILERECF,eq,C'V'
           AHI R0,-4                 EXCLUDE "RDW"  IF VARIABLE
         endif
         STH   R0,FILERECL
*
OPENBRK  llgt  R14,WKFILBRK          LOAD   CURRENT    FILE TYPE BREAK
         ltgr  r1,r14
         BRNP  OPENNEW               YES -  BYPASS     COMPARE
*
         CLC   FILERTYP,FILERTYP-FILEAREA(R14) SAME    FILE  TYPE ???
         BRE   OPENNMAX              YES -  CHECK FOR NEW MAX REC LEN
*
OPENBRKL CLC   FILERTYP-FILEAREA(L'FILERTYP,R14),FILERTYP-FILEAREA(R1)
         BRNE  OPENNEW               BRANCH AT NEXT BREAK
*
         LH    R0,FILEMAXL-FILEAREA(,R14)    MAX   FROM INIT REC ???
         LTR   R0,R0
         BRNP  OPENMAXL              NO  -   USE ACTUAL FILE MAXIMUM
*
         CH    R0,WKMAXLEN           WILL TRUNCATION   OCCUR ???
         BRNL  OPENNEXT              NO  -   CONTINUE
         BRU   OPENTRNC              YES -   DISPLAY   ERROR MSG
*
OPENMAXL MVC   FILEMAXL-FILEAREA(L'FILEMAXL,R14),WKMAXLEN   COPY MAX LN
OPENNEXT LA    R14,FILELEN(,R14)     ADVANCE TO   NEXT FILE AREA
         cgr   R14,R6                REACHED NEW  FILE TYPE ???
         BRL   OPENBRKL              NO  -   LOOP  (FILL-IN MAXIMUM)
*
OPENNEW  ST    R6,WKFILBRK
         MVC   WKMAXLEN,FILERECL
*
OPENNMAX ds    0h
         if CLC,FILERECL,gt,WKMAXLEN NEW    MAXIMUM  LENGTH ???
           MVC WKMAXLEN,FILERECL     YES -  SAVE  LONGER    LENGTH
         endif
         ST    R6,0(,R8)             ADD    ENTRY TO OPEN   FILE LIST
         ST    R8,FILEOPNA           SAVE   ENTRY ADDRESS
         LA    R8,4(,R8)             ADVANCE  TO NEXT OPEN LIST ENTRY
*
*
***********************************************************************
*  LOCATE FIRST DATA BLOCK IN FILE                                    *
***********************************************************************
         ST    R10,WKSAVSUB
         larl  r15,readrec        get the readrec address
         basr  R10,r15            and go there flipping modes as we go
*
         llgt  R10,FILEPRMA          ENTITY DECOMPRESS  ???
         if CLI,PARMDCMP-PARMTBL(R10),eq,C'Y',or,                      +
               CLI,WKPADCMP,eq,C'Y'  PAD DECOMPRESSION  ???
           BRAS R10,LOCVIEW          YES - LOCATE CORRESPONDING VIEW
         endif
         llgt  R10,WKSAVSUB
*
         BRU   INITFNXT
*
INITFAIL ds     0h
         if Cij,R15,ne,8,and,        Not END-OF-FILE ???               +
               CLI,WKIGNRDD,ne,C'Y'  dont IGNORE MISSING DDNAME ERROR ?
*
           MVI WKFAILED,C'Y'         INDICATE ONE OR MORE OPENS TA)
*
*          open of entity file failed
*
           CVD  R15,WKDBLWRK
           OI   WKDBLWRK+L'WKDBLWRK-1,X'0F'
           UNPK WKDBLWK2(3),WKDBLWRK
           Llgt R14,GPENVA

           GVBMSG LOG,MSGNO=OPEN_ENTITY_FILE_FAIL,SUBNO=3,             +
               GENENV=(R14),                                           +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(UR20DDN,L'UR20DDN),                               +
               SUB3=(WKDBLWK2,3),            UR20RC                    +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
         endif
         J     INITEOF
*
INITEOF01 DS   0H
         L     R0,WKLFID
         CVD   R0,WKDBLWRK
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         UNPK  WKDBLWK2,WKDBLWRK
         Llgt  R14,GPENVA
         GVBMSG LOG,MSGNO=CK_PF_DDNAME_ERR2,SUBNO=3,                   +
               GENENV=(R14),                                           +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(WKDDNAME,L'WKDDNAME),                             +
               SUB3=(WKDBLWK2,8),                                      +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
*        WTO   TEXT=WKTXTLEN,MF=(E,WKWTOPRM)
         J     INITEOF
*
INITEOF02 DS   0H
         L     R0,WKLFID
         CVD   R0,WKDBLWRK
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         UNPK  WKDBLWK2,WKDBLWRK
         Llgt  R14,GPENVA
         GVBMSG LOG,MSGNO=CK_NO_READ_EXIT,SUBNO=3,                     +
               GENENV=(R14),                                           +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(WKDDNAME,L'WKDDNAME),                             +
               SUB3=(WKDBLWK2,8),                                      +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*        WTO   TEXT=WKTXTLEN,MF=(E,WKWTOPRM)
*
INITEOF  MVI   FILEEOF,C'Y'          INDICATE END-OF-FILE (NO DATA)
*
*
***********************************************************************
*  CLOSE FILE IF OPEN ISSUED                                          *
***********************************************************************
         MVC   UR20FC,H004        FUNCTION  = "CLOSE"  -  EVENT FILE
*
         LA    R1,UR20PARM
         llgf  R15,UR20ADDR
         if cij,r15,ne,0
           bassm R14,R15
         endif
INITFNXT LA    R6,FILELEN(,R6)       ADVANCE  TO NEXT CONTROL  BLOCK
INITSNXT LA    R5,1(,R5)             ADVANCE  TO NEXT CATEGORY POSITION
         BRCT  R4,INITFSRC
*
         if Cij,R2,ne,1               LAST PARAMETER TABLE ENTRY ???
*
           LA  R3,PARMLEN(,R3)
*
           CLC PARMENT,PARMENT-PARMTBL(R7)  NEW RECORD TYPE   ???
           BRE INITFPNX                   NO  - BYPASS BREAK  LOGIC
*
           lgr R7,R3
         endif
*
         CLI   FILEETYP,C'T'
         BRE   INITFPNX
*
         llgt  R0,WKDIROFF
         aghi  R0,DIRLEN
         ST    R0,WKDIROFF
*
INITFPNX BRCT  R2,INITFILE
*
         llgt  R14,WKFILBRK          LOAD   CURRENT    FILE TYPE BREAK
         ltgr  R1,R14                SAVE   BREAK POINT
         BRNP  OPENFAIL
*
OPENLAST cgr   R14,R6                REACHED NEW  FILE TYPE ???
         BRNL  OPENFAIL              NO  -   LOOP  (FILL-IN MAXIMUM)
*
         CLC   FILERTYP-FILEAREA(L'FILERTYP,R14),FILERTYP-FILEAREA(R1)
         BRNE  OPENFAIL              BRANCH AT NEXT BREAK
*
         LH    R0,FILEMAXL-FILEAREA(,R14) MAX KNOWN FROM INIT REC ???
         LTR   R0,R0
         BRNP  OPENLSTM              NO  -   USE RECORD MAXIMUM
         CH    R0,WKMAXLEN           WILL TRUNCATION   OCCUR ???
         BRNL  OPENNXT2
*
OPENTRNC DS    0H                    DISPLAY "TRUNCATION ERROR" MSG
*        LH    R15,0(,R14)
*        LA    R15,2-1(,R15)
*        exrl  R15,MSGCOPY
*        MVC   WKTXTLEN+2+45(8),UR20DDN-FILEAREA(R1)
         LGR   R2,R1   Current file type break
         GVBMSG FORMAT,MSGNO=CK_RECORD_TRUNC,SUBNO=2,                  +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(UR20DDN-FILEAREA(R2),L'UR20DDN),                  +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
         BRU   RTNERROR
*
OPENLSTM MVC   FILEMAXL-FILEAREA(L'FILEMAXL,R14),WKMAXLEN   COPY MAX LN
OPENNXT2 LA    R14,FILELEN(,R14)     ADVANCE TO   NEXT FILE AREA
         BRU   OPENLAST              NO  -   LOOP  (FILL-IN MAXIMUM)
*
OPENFAIL CLI   WKFAILED,C'Y'      WERE  ALL  OPENS  SUCCESSFUL  ???
         BRNE  INITDPAD           YES - CONTINUE
*
         lghi  R15,16
         BRU   RTNABEND
*
***********************************************************************
*  GET PAD DECOMPRESSION OUTPUT RECORD AREA                           *
***********************************************************************
INITDPAD OC    WKVWLIST,WKVWLIST  DECOMPRESSION SPECIFIED ???
         BRZ   INITEXIT
*
         llgt  R14,WKPLISTA       EVENT FILE DDNAME
         llgt  R14,GPFILEA-GENPARM(,R14)
         USING GENFILE,R14
         lgf   R0,GPRECMAX
         DROP  R14
*
         aghi  R0,8
         GETMAIN RU,LV=(0),LOC=(ANY)
         MVC   0(8,R1),DCMPEYEB
         aghi  R1,8
         ST    R1,WKVWDCMP
*
INITEXIT EQU   *

         sysstate amode64=NO
         sam31
         larl  r15,print_input    print the input data now
         basr  r14,r15
         BR    R10                RETURN
*
         DROP  R3
         DROP  R6
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        L O A D   E X I T   P R O G R A M S                          *
*                                                                     *
*   R0 - CONTAINS EXIT NAME  ADDRESS                                  *
*   R1 - CONTAINS EXIT EXIT  ADDRESS                                  *
*                                                                     *
*   NOTE:                                                             *
*   The LOAD macro returns the entry point address of the requested   *
*   load module in R0. The Load service sets bits within the entry    *
*   point address to indicate the load module's AMODE:                *
*                                                                     *
*     AMODE 24: Within the 32-bit GPR, the high-order and low-order   *
*       bits are both 0.                                              *
*     AMODE 31: Within the 32-bit GPR, the high-order bit is 1,       *
*       low-order bit is 0.                                           *
*     AMODE 64: The 64-bit GPR contains the entry point address.      *
*       Bit 63 is 1.                                                  *
*                                                                     *
*     If the module's AMODE is ANY, it indicates AMODE 24 if the      *
*     caller is AMODE 24 (the high order bit is 0), or AMODE 31       *
*     if the caller is AMODE 31 or AMODE 64 (the high order bit is 1) *
*                                                                     *
*     During initialization EXITLOAD is AMODE 31                      *
*     Exits can be AMODE 31 or AMODE 64, and RMODE 24 or RMODE 31     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
EXITLOAD stmg  R14,R12,WKSAVSUB        SAVE CALLING SECTION'S REGISTERS
*
         LR    R2,R0              SAVE EXIT name ADDRESS
         LR    R3,R1              SAVE EXIT EXIT ADDRESS
*
         LOAD  EPLOC=(0),ERRET=EXITERR
*
*        Oilh  R0,MODE31          FORCE 31-BIT     MODE
         ST    R0,0(,R3)          SAVE  SUBROUTINE ADDRESS
*
         lmg   R14,R12,WKSAVSUB        LOAD CALLING SECTION'S REGISTERS
         BR    R14
*
EXITERR  DS    0h
*        unable to load exit error
         GVBMSG FORMAT,MSGNO=CK_LOAD_EXIT_FAIL,SUBNO=2,                +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(0(,R2),8),        Exit name                       +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR
*
                        EJECT
         sysstate amode64=YES
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* C O P Y   I N P U T   R E C O R D   T O   O U T P U T   B U F F E R *
*                (PERFORM ONE-UP AND TOTAL COLLAPSE)                  *
*                                                                     *
*   R2 - CONTAINS OUTPUT  RECORD  ADDRESS                             *
*   R6 - CONTAINS FILE    CONTROL BLOCK  ADDRESS                      *
*   R9 - CONTAINS CURRENT OUTPUT  BUFFER POSITION                     *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING FILEAREA,R6
*
OUTPUT   LGH   R15,FILERECL       LOAD   EVENT   RECORD  LENGTH
         if CLI,FILERSRC,eq,C'S'  SUBSTITUTION   RECORD  ???
           BCTR R15,0             EXCLUDE ACTION CHARACTER
         endif
         STH   R15,WKOUTLEN       SAVE  OUTPUT   RECORD  LENGTH
*
OUTPUT0  LGF   R8,FILEVREC        LOAD DECOMPRESSED VIEW DEFN ADDRESS
         LTGR  R8,R8
         BRNP  OUTPUTT
         OC    VWMAXCOL-VIEWREC(L'VWMAXCOL,R8),VWMAXCOL-VIEWREC(R8)
         BRZ   OUTPUTT
         BRAS  R14,NORMCT         NORMALIZE "CT" COLUMNS
*
*
***********************************************************************
*   CHECK IF RECORD GOES TO TRANSACTION BUFFER/BLOCK                  *
***********************************************************************
OUTPUTT  EQU   *
*        LH    R15,FILERECL
*        AR    R15,R2
*        BCTR  R15,0
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=REGS,ID=207,STORAGE=((R2),(R15))
*
         CLI   FILEETYP,C'T'      UNBUFFERED TRANSACTION RECORD ??
         BRE   OUTPTRN
*
         CLI   FILENEWB,C'Y'      ENTITY BEGINS  NEW TRAN BLOCK  ???
         BRNE  OUTPUTE
*
         LTG   R14,WKRECPRV       ANY  PREV RECS FOR THIS ENTITY ???
         BRP   OUTPTRN            YES - CONTINUE EXISTING BLOCK
*
         LG    R14,WKTRNEND       LOAD CURRENT DATA  ENDING  ADDRESS
         SG    R14,WKTRNBDW       COMPUTE BLOC LENGTH
         Cgijnh R14,8,outptrn     ALREADY A NEW BLOCK  ???
*
***********************************************************************
*   RETURN EXISTING TRANSACTION BLOCK BEFORE STARTING A NEW ONE       *
***********************************************************************
         ST    R2,WKTCONT2        INDICATE TRANSACTIONS CONTINUED
         ST    R3,WKTCONT3
         ST    R5,WKTCONT5
         STG   R9,WKTCONT9
         ST    R10,WKTCONTA
*
         BRU   MERGBLK1
*
*
***********************************************************************
*   RECORD WRITTEN TO "LR BUFFER" ENTITY SECTION                      *
***********************************************************************
OUTPUTE  LGR   R1,R9              COMPUTE LENGTH OF DATA IN BUFFER
         SG    R1,WKBLKBDW
*
         LGH   R15,FILEMAXL       USE EXTENDED   LEN FOR OVERFLOW CHECK
*
         LA    R0,4+L'FILEPRFX(R9,R15)  BUFFER OVERFLOW ???
         CG    R0,WKBLKMAX
         jnh   outpute_05         N: then carry on
* Y: then issue relevant message
         GVBMSG FORMAT,MSGNO=CK_BUFFER_OVERFLOW,SUBNO=3,               +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(UR20DDN,L'UR20DDN),                               +
               SUB3=(0(r2),24),                  record/key            +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR
*
outpute_05 ds  0h
         LA    R0,4+L'FILEPRFX(R1,R15)  BLOCK  OVERFLOW ???
         Cfi   R0,BLKSIZE
         BRNH  OUTPCHKP           NO  - CONTINUE
***********************************************************************
*  CLOSE OUT PREVIOUS BLOCK (BDW) AND START NEW BLOCK (BDW)           *
***********************************************************************
OUTPBLK  LGR   R15,R9             LOAD END-OF-DATA  ADDRESS
         LG    R1,WKBLKBDW        COMPUTE  BLOCK    LENGTH
         SGR   R15,R1
         STH   R15,0(,R1)
*
         STG   R9,WKBLKBDW        START NEW  BDW
         XGR   R0,R0
         STG   R0,0(,R9)
         AGHI  R9,4
         STG   R9,WKBLKEND
*
         if CG,R9,gt,WKBLKHI         NEW "HIGHWATER MARK" ???
           STG R9,WKBLKHI
         endif
*
         LH    R15,WKOUTLEN       LOAD OUTPUT RECORD   LENGTH
         BRU   OUTPUTT            COPY RECORD INTO NEW BLOCK
***********************************************************************
*  CHECK IF NON-PROFILE OR PROFILE RECORD                             *
*                                                                     *
*  IF     PROFILE: CHECK  IF CORRESPONDING PROFILE ENTITY ID ASSIGNED *
*  IF NON-PROFILE: CHECK  IF PROFILE  ID LINK ALREADY FILLED-IN       *
*           (XHEXFF INDICATES NO ASSIGNED PROFILE ID LINK)            *
*                                                                     *
*       ("COLLAPSE-ALL" MEANS REASSIGN PROFILE ID'S)                  *
***********************************************************************
OUTPCHKP CLI   FILEETYP,C'P'      PROFILE  RECORD ???
         BRNE  OUTPNONP           NO  - CHECK FOR FILLED-IN  PROFILE ID
*
         LGH   R15,FILEPROO       PROFILE ID  PRESENT IN  RECORD ???
         LTgr  R15,R15
         BRNP  OUTPDUPL           NO  - CHECK IF "ONE UP" COLLAPSE
*
         LA    R14,0(R2,R15)      LOAD  PROFILE  ID   ADDRESS
*
         CLI   FILECLAP,C'A'      "COLLAPSE-ALL" ???
         BRE   OUTPPROF           YES - REASSIGN PROFILE ID
*
         CLC   0(L'PROIDCUR,R14),XHEXFF PROFILE ID ALREADY ASSIGNED ???
         BRNE  OUTPDUPL                YES -   CHECK   IF DUPLICATE
*
*
***********************************************************************
*        P R O F I L E   E N T I T Y   P R O C E S S I N G            *
*                                                                     *
*  SEARCH PRIOR RECORDS FOR   THIS PROFILE ENTITY  FOR SAME CONTENT   *
*  IF FOUND:  TABLE THE PROFILE ID ALREADY ASSIGNED TO THIS CONTENT   *
*  IF NOT:   ASSIGN NEXT AVAILABLE PROFILE ID NUMBER (1 IF FIRST)     *
***********************************************************************
OUTPPROF MVC   WKPROOLD,0(R14)    SAVE OLD/INCOMING ID (MAY BE X'FF')
*
         llgt  R8,FILEMINA        LOAD DIRECTORY ENTRY ADDR
         llgt  R8,0(,R8)          LOAD FIRST    RECORD POINTER   ADDR
         cgf   R8,WKLSTCUR        ANY  PROFILE RECORDS AVAILABLE ???
         BRNL  OUTPPRO1           NO  - ASSIGN FIRST
*
OUTPPROL llgt  R1,0(,R8)          LOAD PROFILE RECORD  ADDRESS
*
         LH    R0,4+L'FILEPRFX(R1,R15) SAVE POTENTIAL  NEW  ID
         STH   R0,WKPRONEW
*
         LHI   R0,4+L'FILEPRFX    PREFIX   LENGTH
         AH    R0,WKOUTLEN        COMPUTE  TOTAL  LENGTH
         CH    R0,0(,R1)          MATCHING LENGTH ???
         BRNE  OUTPPRON           NO  - RECORDS   NOT  THE SAME
*
         CH    R15,FILEPRTL       MINIMUM OFFSET  EXCEEDED  ???
         BRNH  OUTPMTCH
*
         LGH   R14,FILEPRTL       LOAD EMBEDDED   PARTITION LENGTH
         SGR   R15,R14            EXCL PARTITION ID
         LA    R1,4+L'FILEPRFX(R1,R14)
         LA    R14,0(R2,R14)
         BCTR  R15,0              DECREMENT FOR "EX"
         exrl  R15,CLCR1R14
         BRNE  OUTPPRON
*
OUTPMTCH LGH   R15,FILETMSO       LOAD    TIMESTAMP OFFSET
         AGHI  R15,L'WKTSTMP      ADD     OFFSET TO DATA   (EXCL TSTMP)
         LA    R14,4+L'FILEPRFX(,R15)     EXISTING  RECORD
         AG    R14,0(,R8)
         LGH   R1,WKOUTLEN        COMPUTE REMAINDER LENGTH (DATA LEN)
         SGR   R1,R15
         LA    R0,0(R2,R15)       LOAD DATA ADDRESS (BEYOND PROFILE ID)
         LGR   R15,R1
         CLCL  R0,R14             COMPARE CONTENT  BEYOND TIMESTAMP
         BRE   OUTPPROE           BRANCH  IF  EQUAL
*
OUTPPRON LGR   R14,R8             SAVE    PREVIOUS RECORD POINTER ADDR
         AGHI  R8,8               ADVANCE TO  NEXT RECORD POINTER
         LGH   R15,FILEPROO       RELOAD  PROFILE  ID     OFFSET
         C     R8,WKLSTCUR        LAST PROFILE RECORD ???
         BRL   OUTPPROL           NO  -  LOOP
*
OUTPPROI LGH   R1,WKPRONEW        LOAD LAST NUMBER ASSIGNED
         Aghi  R1,1               INCREMENT BY ONE
         BRU   OUTPPROA
*
OUTPPRO1 LGHI  R1,1
OUTPPROA lt    R0,FILEPXEP        CALL "PROFILE ID" EXIT IF SPECIFIED
         BRZ   OUTPPROX
*
         XC    WKEXITRC,WKEXITRC  ZERO  RETURN  CODE
         stg   R2,WKCALLPT        RECORD  ADDRESS
         ST    R1,WKCALLRL        PROFILE ID
*
         Llgt  R14,FILEPRMA       RELATED PARMTBL ENTRY
         LA    R1,PARMPXPM-PARMTBL(,R14)
         ST    R1,WKCALLP3
*
         LAY   R1,WKCALL64        Load pointer to Event Record address
         STY   R1,WKCALLP4        Store pointer in parm list
*
         LA    R1,FILEPXAN        WORK  AREA  ANCHOR
         ST    R1,WKCALLP7
         LA    R1,WKEXITRC        RETURN  CODE
         ST    R1,WKCALLP8
*
         LA    R1,WKCALLPL
         llgf  R15,FILEPXCA
         bassm R14,R15
*
         LGF   R1,WKCALLRL        LOAD  ASSIGNED PROFILE ID
         LGH   R15,FILEPROO       RELOAD PROFILE ID  OFFSET
*
OUTPPROX STH   R1,0(R2,R15)       SAVE   PROFILE ID
         STH   R1,WKPRONEW
*
         LGR   R1,R2
         BRAS  R14,OUTPTABL
*
         BRU   OUTPPREV           OUTPUT PROFILE RECORD
*
OUTPPROE LGH   R15,FILEPROO       CLONE EXISTING ID
         LGH   R0,WKPRONEW
         STH   R0,0(R2,R15)
*
*
***********************************************************************
*  DON'T CREATE DUPLICATE TABLE ENTRY IF SAME TIMESTAMP               *
***********************************************************************
         LG    R14,0(,R8)         CHECK IF SAME TIMESTAMP ???
         LGH   R1,FILETMSO
         AGR   R14,R1
         AGR   R1,R2
         CLC   0(L'WKTSTMP,R14),0(R1)
         BRE   OUTPDUPE           YES - DON'T TABLE DUPLICATE TSTAMP
*
         LGR   R1,R2
         BRAS  R14,OUTPTABL       TABLE PROFILE ID + TIMESTAMP
*
         BRU   OUTPDUPE
                        EJECT
***********************************************************************
*        N O N - P R O F I L E   R E C O R D   P R O C E S S I N G    *
*                                                                     *
*  FIND MATCHING PROFILE ID ASSIGNMENT TABLE ENTRY USING TSTMP/OLD ID *
*  COPY PROFILE  ID INTO PROFILE  RECORD                              *
***********************************************************************
OUTPNONP LGH   R1,FILEPROO        LOAD  PROFILE ID  OFFSET
         LTGR  R1,R1              CORRESPONDING PROFILE     ENTITY  ???
         BRNP  OUTPDUPL           NO  -  BYPASS SEARCH
*
         AGR   R1,R2
         CLC   0(L'PROIDCUR,R1),XHEXFF   PROFILE ID ALREADY PRESENT ???
         BRE   OUTPTBLS                  NO  - SEARCH TABLE
*
         MVC   WKPRONEW,0(R1)     SAVE  ASSIGNED PROFILE ID
*
         CLI   FILECLAP,C'Y'      "COLLAPSE-ALL" ???
         BRNE  OUTPDMAT           NO - CHECK IF PROFILE ID TSTMP UPDATE
*
OUTPTBLS llgt  R14,WKPROBEG       LOAD   PROFILE ID   TABLE ADDRESS
         USING PROIDTBL,R14
*
OUTPTBLL cgf   R14,WKPROEND       END-OF-TABLE   ???
         BRNL  OUTPNODT           YES -  NO  MATCHING TIMESTAMP
*
         CLC   0(L'PROIDCUR,R1),XHEXFF   PROFILE ID ALREADY PRESENT ???
         BRNE  OUTPTBLO                  NO  - SEARCH TABLE
         LGH   R15,FILETMSO       MATCHING  TIMESTAMP ???
         AGR   R15,R2
         CLC   PROTSTMP,0(R15)
         BRE   OUTPPIDF           YES - UPDATE PROFILE ID IN PROFILE
         LA    R14,PROTBLEN(,R14)
         BRU   OUTPTBLL
*
OUTPTBLO CLC   0(L'PROIDCUR,R1),PROIDOLD MATCH ON OLD PROFILE ID ???
         BRE   OUTPPIDF           YES - UPDATE PROFILE ID IN PROFILE
         LA    R14,PROTBLEN(,R14)
         BRU   OUTPTBLL
*
OUTPNODT CLC   0(L'PROIDCUR,R1),XHEXFF PROFILE ID ALREADY THERE ???
         BRNE  OUTPDUPL           YES - NOT AN ERROR   IF NOT FOUND
*
*     DISPLAY "NO MATCHING TIMESTAMP"
         llgt  R3,FILEPRMA
         using PARMTBL,r3
         llgt  R2,WKBRKID
         GVBMSG FORMAT,MSGNO=CK_PROFILE_TIMESTAMP_NF,SUBNO=2,          +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(PARMENT,L'PARMENT),         entity ID             +
               SUB3=(0(R2),6),                   break ID              +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         drop  r3
         BRU   RTNERROR
*
OUTPPIDF LGH   R15,PROIDCUR       FILL-IN MATCHING PROFILE ID
         STH   R0,0(,R1)
         STH   R0,WKPRONEW
*
*
         DROP  R14
                        EJECT
OUTPDMAT MVC   WKPROOLD,WKPRONEW  SAVE/PASS  PROFILE ID (PARAMETER)
*
         BRAS  R14,OUTPTSTM       UPDATE TIMESTAMP IF THIS ONE EARLIER
*
*
***********************************************************************
*        C O M M O N   L O G I C   (O N E   U P   C O L L A P S E)    *
*                                                                     *
*  CHECK FOR DUPLICATE OF PREVIOUS RECORD (EXCLUDING TIMESTAMP)       *
***********************************************************************
OUTPDUPL CLI   FILEETYP,C'T'      NON-BUFFERED  TRANSACTION ENTITY ???
         BRE   OUTPPREV           YES -  DON'T  COLLAPSE
*
         LTG   R14,WKRECPRV       PREVIOUS RECORD AVAILABLE ???
         BRNP  OUTPPREV
*
         CLI   FILECLAP,C'1'      1-UP COLLAPSE   ???
         BRE   OUTP1UP
         CLI   FILECLAP,C'M'      MARK DUPLICATES ???
         BRE   OUTPCKKY           YES - COMPARE   PREV/NEW KEYS
*
         CLI   FILEKMAP,X'00'     LAST KEY VALUE MAPPING SPECIFIED
         BRE   OUTPPREV           NO - OUTPUT RECORD
*
OUTPCKKY LGH   R15,FILEKEYO       COMPUTE KEY ADDRESS (NEW  RECORD)
         LA    R0,0(R2,R15)
         LA    R14,4+L'FILEPRFX(R14,R15)  KEY ADDRESS (PREV RECORD)
         LGH   R15,FILEKEYL       LOAD    KEY LENGTH
         LGR   R1,R15
         CLCL  R14,R0             SAME  KEY   VALUE ???
         BRNE  OUTPKMAP           NO  - COPY  RECORD TO BUFFER
*
***********************************************************************
*  WHEN KEYS ARE EQUAL USE EFFECTIVE DATE AS "TIE BREAKER"            *
***********************************************************************
         LG    R14,WKRECPRV       PREVIOUS  RECORD ADDRESS
         LGH   R1,FILEEFDO        EFFECTIVE DATES  PRESENT  ???
         LTGR  R1,R1
         BRM   OUTPKMAP           NO - MARK PREVIOUS KEY AS DUPLICATE
*
         LA    R15,0(R2,R1)       COMPUTE NEW  REC EFF DATE ADDRESS
         LA    R1,4+L'FILEPRFX(R14,R1)    PREV REC EFF DATE ADDRESS
         CLC   0(4,R1),0(R15)     DIFF  EFFECTIVE DATE ???
         BRNE  OUTPPREV
*
OUTPKMAP DS    0H                                                 @019I
         LG    R14,WKRECPRV       RESTORE PREVIOUS RECORD  ADDR   @019I
         CLI   FILECLAP,C'M'      MARK  PREVIOUS AS DUPLICATE     @019C
         BRE   OUTPMARK
*
         CLI   FILEKMAP,X'00'     MAP  LAST KEY VALUE RECORD ??
         BRE   OUTPPREV           NO - LEAVE AS IS
         MVC   4+L'FILEPRFX-1(1,R14),FILEKMAP MARK LAST KEY VALUE REC
         BRU   OUTPPREV
*
OUTPMARK MVI   4+L'FILEPRFX-1(R14),C'X' MARK  PREVIOUS AS DUPLICATE
         BRU   OUTPPREV
*
OUTP1UP  LGHI  R0,4+L'FILEPRFX    RDW + PREFIX
         AH    R0,WKOUTLEN
         CH    R0,0(,R14)         MATCHING LENGTH ??
         BRNE  OUTPPREV           NO  - RECORDS  NOT THE   SAME
*
         LGH   R1,FILETMSO        LOAD TIMESTOP  OFFSET
         LTGR  R1,R1              TIMESTAMP PRESENT ???
         BRNP  OUTPDUPR           NO - COMPARE   FULL   RECORD
*
         LGH   R0,FILEPRTL        LOAD EMBEDDED  PARTITION LEN
         SGR   R1,R0              EXCL PARTITION ID
         AGHI  R14,4+L'FILEPRFX   SKIP PREFIX
         AGR   R14,R0             SKIP EMBEDDED  PARTITION
         AGR   R0,R2
         LGR   R15,R1
         CLCL  R0,R14             MATCHING KEY   ???
         BRNE  OUTPPREV           NO  -    NOT   A   DUPLICATE
*
         AGHI  R0,L'WKTSTMP       SKIP TIMESTAMP
         AGHI  R14,L'WKTSTMP
         LGH   R15,WKOUTLEN       COMPUTE  REMAINDER LENGTH
         LGH   R1,FILETMSO
         AGHI  R1,L'WKTSTMP
         SGR   R15,R1
         LGR   R1,R15
         CLCL  R0,R14             MATCHING REMAINDER ???
         BRE   OUTPDUPE           YES -    IGNORE RECORD
         BRU   OUTPPREV           NO  -    OUTPUT RECORD
*
OUTPDUPR LGH   R1,FILEPRTL        LOAD EMBEDDED   PARTITION LEN
         AGHI  R14,4+L'FILEPRFX   SKIP PREFIX
         AGR   R14,R1
         LGH   R15,WKOUTLEN       COMPUTE  REMAINDER LENGTH
         SGR   R15,R1
         AGR   R1,R2
*
         LA    R15,255(,R15)
         SRLG  R0,R15,8
         BRU   OUTPCLCT
OUTPCLLP CLC   0(256,R1),0(R14)   COMPARE  256 BYTES
         BRNE  OUTPPREV
         LA    R1,256(,R1)        ADVANCE  TARGET
         LA    R14,256(,R14)      ADVANCE  SOURCE
OUTPCLCT BRCT  R0,OUTPCLLP
         exrl  R15,CLCR1R14       COMPARE  REMAINDER
         BRNE  OUTPPREV
*
OUTPDUPE agsi  FILEDCNT,b001      INCREMENT  DUPLICATE  COUNT
         BRU   OUTPEXIT           BYPASS     WRITE
*
*
***********************************************************************
*  UPDATE PREVIOUS  RECORD    ADDRESS                                 *
*  ADD    NEW ENTRY TO RECORD ADDRESS LIST                            *
*  BUILD  OUTPUT    RECORD   (INSERT  PREFIX)                         *
***********************************************************************
OUTPPREV STG   R9,WKRECPRV        UPDATE  SAME ENTITY PREVIOUS REC ADDR
         STG   R9,WKLSTREC        UPDATE  LAST RECORD  ADDRESS
*
         llgt  R14,WKLSTCUR       ADD   RECORD ADDRESS TO LIST
         STG   R9,0(,R14)
         Aghi  R14,8              ADVANCE LIST ADDRESS
         Cgf   R14,WKLSTMAX       CHECK   FOR OVERFLOW
         BRL   OUTPRPTR
*
*   Record pointer table overflow
         llgt  R2,WKBRKID
         GVBMSG FORMAT,MSGNO=CK_REC_PTR_TABLE_OVERFLOW,SUBNO=2,        +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(0(R2),11),                  break ID              +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR
*
OUTPRPTR ST    R14,WKLSTCUR
*
         LGH   R1,FILEMAXL
         if  CLI,FILERECF,ne,C'F'
           LGH R1,WKOUTLEN
         endif
         LA    R0,4+L'FILEPRFX(,R1)
         STH   R0,0(,R9)          BUILD  RDW
         XC    2(2,R9),2(R9)
         MVC   4(L'FILEPRFX,R9),FILEPRFX
*
         if CLI,FILERSRC,eq,C'S'      SUBSTITUTION RECORD ???
           MVI 4+L'FILEPRFX-1(R9),C'D'  MARK AS DAILY
         endif
*
         LA    R1,4+L'FILEPRFX(,R9)
         LGR   R14,R2
         LGH   R15,WKOUTLEN
         LA    R15,255(,R15)
         SRLG  R0,R15,8
         BRU   OUTPMVCT
OUTPMVLP MVC   0(256,R1),0(R14)   COPY  256 BYTES
         LA    R1,256(,R1)        ADVANCE  TARGET
         LA    R14,256(,R14)      ADVANCE  SOURCE
OUTPMVCT BRCT  R0,OUTPMVLP
         exrl  R15,MVCR1R14       COPY  REMAINDER
*
*
***********************************************************************
*  EXTEND RECORDS WITH INITIAL VALUES IF SHORT                        *
***********************************************************************
         SLL   R15,24             CLEAR HIGH   LENGTH    BYTES
         SRL   R15,24
         LA    R1,1(R1,R15)       ADVANCE TARGET ADDR
*
         ltgf  R14,FILEINIA       LOAD  INITIALIZATION   RECORD ADDRESS
         BRNP  OUTPEND            NO  - BYPASS PADDING   LOGIC
*
         LGH   R15,FILEMAXL       ANY  PADDING NECESSARY ???
         SH    R15,WKOUTLEN
         BRNP  OUTPEND            NO - GENERATE  VIRTUAL RECORD
*
         LH    R0,0(,R9)          UPDATE RDW
         AR    R0,R15
         STH   R0,0(,R9)
*
         lgh   R0,FILEMAXL        POINT   TO END OF INITIALIZATION  REC
         agr   R14,R0
         Sgr   R14,R15            BACKUP  TO BEGINNING OF NEEDED PADING
*
         LA    R15,255(,R15)      INITIALIZE REMAINDER OF RECORD
         SRLG  R0,R15,8
         BRU   OUTPINCT
OUTPINLP MVC   0(256,R1),0(R14)   COPY  256 BYTES
         LA    R1,256(,R1)        ADVANCE  TARGET
         LA    R14,256(,R14)      ADVANCE  SOURCE
OUTPINCT BRCT  R0,OUTPINLP
         exrl  R15,MVCR1R14       COPY  REMAINDER
*
*
***********************************************************************
*  UPDATE CURRENT "END-OF-LOGICAL-RECORD" BUFFER(NEXT OUTPUT REC ADDR)*
***********************************************************************
OUTPEND  EQU   *
*        LH    R15,0(,R9)
*        AR    R15,R9
*        BCTR  R15,0
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),ID=216,STORAGE=((R9),(R15))
*
         LGH   R15,0(,R9)
         AGR   R9,R15
         STG   R9,WKBLKEND
*
         if CG,R9,gt,WKBLKHI         NEW "HIGHWATER MARK" ???
           STG R9,WKBLKHI
         endif
*
*   WRITE TRACE ERROR MESSAGE  (OUTPUT   RECORD)
*        LR    R14,R0
*        MVC   ERRFMTID,FILERTYP
*        MVC   ERRKEY,0(R14)
*        MVC   ERRTEXT,OUTPMSG
*        L     R2,WKRPTDCB
*        PUT   (R2),ERRORMSG
*
OUTPEXIT BR    R10
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
* R E T U R N   U N B U F F E R R E D   T R A N S A C T I O N   R E C *
*            (APPEND TO RESERVED BLOCK AT END OF "LR BUFFER")         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
OUTPTRN  EQU   *
*        LH    R15,WKOUTLEN
*        AR    R15,R2
*        BCTR  R15,0
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),ID=217,STORAGE=((R2),(R15))
*
         LG    R1,WKTRNEND        COMPUTE LENGTH OF DATA IN BUFFER
         LGR   R0,R1
         LG    R14,WKTRNBDW
         SGR   R0,R14
*
         LGH   R15,WKOUTLEN       LOAD  EVENT    RECORD  LENGTH
         AGHI  R15,4+L'FILEPRFX   ADD   PREFIX   LENGTH
*
         AGR   R0,R15             BLOCK OVERFLOW ???
         Cgfi  R0,BLKSIZE
         BRNH  OUTPTRN1
*
         ST    R2,WKTCONT2        INDICATE TRANSACTIONS CONTINUED
         ST    R3,WKTCONT3
         ST    R5,WKTCONT5
         STG   R9,WKTCONT9
         ST    R10,WKTCONTA
*
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=REGS,ID=209
*
         BRU   MERGBLK1
*
OUTPTRN1 STH   R0,0(,R14)         UPDATE "BDW"
*
         STH   R15,0(,R1)         BUILD  "RDW"
         XC    2(2,R1),2(R1)
         MVC   4(L'FILEPRFX,R1),FILEPRFX
*
         STG   R1,WKRECPRV        UPDATE PREVIOUS RECORD ADDRESS
*
         LA    R1,4+L'FILEPRFX(,R1)
         LGR   R14,R2
         AGHI  R15,-4-L'FILEPRFX
         LA    R15,255(,R15)
         SRLG  R0,R15,8
         BRU   OUTPTRN9
OUTPTRN8 MVC   0(256,R1),0(R14)   COPY  256 BYTES
         LA    R1,256(,R1)        ADVANCE  TARGET
         LA    R14,256(,R14)      ADVANCE  SOURCE
OUTPTRN9 BRCT  R0,OUTPTRN8
         exrl  R15,MVCR1R14       COPY  REMAINDER
*
         SLL   R15,24             CLEAR HIGH ODER LENGTH BYTES
         SRL   R15,24
         LA    R0,1(R1,R15)       UPDATE  CURRENT ENDING ADDRESS
         STG   R0,WKTRNEND
*
         BR    R10
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        T A B L E   P R O F I L E   I D ' S   A S S I G N E D        *
*                                                                     *
*   R1  - CONTAINS OUTPUT RECORD  ADDRESS                             *
*   R6  - CONTAINS FILE   AREA    ADDRESS                             *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
OUTPTABL LGR   R0,R1              SAVE  DATA   ADDRESS
*
         MVC   WKPROSAV(PROTBLEN),XHEXFF
*
         llgt  R15,WKPROBEG       LOAD  FIRST  ENTRY ADDRESS
         Cgf   R15,WKPROEND       ANY   ENTRIES   IN TABLE ???
         BRNL  OUTPTBL6           NO  - INSERT FIRST ENTRY
*
OUTPTBL2 LGH   R1,FILEPROO
         AGR   R1,R15
         if CLC,WKPRONEW,eq,0(R1)     CLONING SOURCE PROFILE ID ???
           MVC WKPROSAV(PROTBLEN),0(R15)
         endif
         LGH   R1,FILEKEYO        * * * BREAK ID * * *
         AGR   R1,R0
         CLC   PROBRKID-PROIDTBL(L'PROBRKID,R15),0(R1)
         BRNE  OUTPTBL4
*
         LGH   R1,FILETMSO        * * * TIMESTAMP * * *
         AGR   R1,R0
         CLC   PROTSTMP-PROIDTBL(L'PROTSTMP,R15),0(R1)
         BRE   OUTPTBL8
*
OUTPTBL4 LA    R15,PROTBLEN(,R15) ADVANCE TO NEXT TABLE ENTRY
         cgf   R15,WKPROEND       END OF TABLE ???
         BRL   OUTPTBL2           NO  - LOOP
*
OUTPTBL6 MVC   0(PROTBLEN,R15),WKPROSAV
*
         LGH   R1,FILEKEYO        * * * BREAK ID * * *
         AGR   R1,R0
         MVC   PROBRKID-PROIDTBL(L'PROBRKID,R15),0(R1)
*
         LGH   R1,FILETMSO        * * * TIMESTAMP * * *
         AGR   R1,R0
         MVC   PROTSTMP-PROIDTBL(L'PROTSTMP,R15),0(R1)
*
         LA    R0,PROTBLEN(,R15) ADVANCE TO NEXT TABLE ENTRY
         cgf   R0,WKPROMAX
         BRNL  OUTPTERR
         ST    R0,WKPROEND
*
OUTPTBL8 LH    R0,WKPRONEW        * * * CURRENT/NEW PROFILE ID * * *
         STH   R0,PROIDCUR-PROIDTBL(,R15)
*
         LH    R0,WKPROOLD        * * * OLD PROFILE ID * * *
         STH   R0,PROIDOLD-PROIDTBL(,R15)
*
         BR    R14                RETURN
*
OUTPTERR DC    0H                 DISPLAY "PROFILE ID TABLE OVERFLOW"
         llgt  R2,WKBRKID
         GVBMSG FORMAT,MSGNO=CK_PROFILE_ID_TABLE_OVERFLOW,SUBNO=2,     +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(0(R2),6),                   break ID              +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
         BRU   RTNERROR
                        EJECT
***********************************************************************
*                                                                     *
*  FIND PROFILE RECORD WITH MATCHING PROFILE ID FROM NON-PROFILE      *
*  UPDATE PROFILE RECORD WITH TIMESTAMP IF EARLIER                    *
*                                                                     *
*        R1  - RECORD TYPE TABLE ENTRY ADDRESS ("RECTYPTB")           *
*                                                                     *
***********************************************************************
OUTPTSTM LGR   R0,R14             SAVE   RETURN  ADDRESS
*
         MVC   WKTSTMP,XHEXFF     CLEAR  SAVE    AREA
*
         CLC   WKPROOLD,XHEXFF
         BRE   OUTPTSTX
*
         llgt  R14,FILEMINA       LOAD   NON-PROFILE ENTITY DIRECTORY
         AGHI  R14,-8             BACKUP TO  PROFILE ENTITY
         llgt  R8,0(,R14)
         Cgf   R8,4(,R14)         ANY    PROFILE RECORDS AVAILABLE ???
         BRNL  OUTPTSTX           NO  -  BYPASS  TIMESTAMP  UPDATE
*
OUTPTSTL LGH   R15,FILEPROO-FILEAREA(,R1)
         AG    R15,0(,R8)
         AGHI  R15,4+L'FILEPRFX
         CLC   0(L'PROIDCUR,R15),WKPROOLD
         BRE   OUTPTSTU           COMPARE TIMESTAMPS IF EQUAL
*
         AGHI  R8,8               ADVANCE TO   NEXT  RECORD
         CGF   R8,4(,R14)         LAST    RECORD ???
         BRL   OUTPTSTL           NO  -   LOOP
*
         llgt  R14,FILEPRMA       Address parmtable
         using PARMTBL,r14
         llgt  R2,WKBRKID
         LH    R0,WKPROOLD        LOAD  PROFILE ID
         CVD   R0,WKDBLWRK
         UNPK  WKDBLWK2(4),WKDBLWRK
         OI    WKDBLWK2+3,X'F0'
*
         GVBMSG FORMAT,MSGNO=CK_PROFILE_ID_NF,SUBNO=4,                 +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(PARMENT,l'PARMENT),             entity            +
               SUB3=(0(r2),6),                       break ID          +
               SUB4=(WKDBLWK2,4),                    Profile ID        +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
         drop  r14
         BRU   RTNERROR
*
***********************************************************************
*  CHECK IF EARLIER TIMESTAMP ON INCOMING RECORD                      *
***********************************************************************
OUTPTSTU LGH   R14,FILETMSO-FILEAREA(,R1)
         AG    R14,0(,R8)
         AGHI  R14,4+L'FILEPRFX   PREVIOUS TIMESTAMP
*
         LGH   R15,FILETMSO       LOAD  TIMESTAMP   OFFSET
         AGR   R15,R2                               INCOMING TIMESTAMP
*
         CLC   0(L'WKTSTMP,R14),0(R15) PREVIOUS <= INCOMING ???
         BRNH  OUTPTSTX                 YES - BYPASS  UPDATE
         MVC   WKTSTMP,0(R14)           NO  - SAVE    ORIGINAL   VALUE
         MVC   0(L'WKTSTMP,R14),0(R15) NO   - CHANGE  TIMESTAMP
*
OUTPTSTX LGR   R14,R0             RETURN
         BR    R14
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        N O R M A L I Z E   "C T"   C O L U M N S                    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING VIEWREC,R8
         USING FILEAREA,R6
         USING EXTREC+4,R2
*
NORMCT   STMg  R14,R12,savf4sag64rs14    Save in the work area - NOTE  +
                            this means NO services in this routine
*
*        LH    R15,0(,R2)
*        BCTR  R15,0
*        AR    R15,R2
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),ID=100,STORAGE=((R2),(R15))
*
         llgt  R5,WKVWDCMP        LOAD TARGET  RECORD ADDRESS
*
***********************************************************************
*  COPY RECORD UP TO "CT" COLUMNS                                     *
***********************************************************************
         LGH   R3,EXSORTLN-EXSORTLN(,R2)    LENGTH  EXCL "CT" COLUMNS
         LA    R3,EXSORTKY-EXSORTLN(R2,R3)
         LGH   R4,EXTITLLN-EXSORTLN(,R2)
         AGR   R3,R4
*
         LGH   R0,EXDATALN-EXSORTLN(,R2) CHECK FOR EXTRACT HEADER REC
         if LTGR,R0,R0,m
           LCGR R0,R0
         endif
         AGR   R3,R0
*
         LGR   R4,R3              SAVE   SOURCE   "CT" COLUMN ADDRESS
         SGR   R3,R2              OFFSET TO "CT"   COLUMNS
*
         LGR   R1,R2              LOAD   SOURCE    ADDRESS
         LGR   R14,R5             LOAD   TARGET    ADDRESS
*
         LA    R15,255(,R3)       ROUND  UP TO 256 MULTIPLE
         SRLG  R0,R15,8           LENGTH OF RECORD TO  MOVE
         BRU   COPYMVCE
COPYMVCL MVC   0(256,R14),0(R1)
         LA    R1,256(,R1)
         LA    R14,256(,R14)
COPYMVCE BRCT  R0,COPYMVCL
         exrl  R15,MVCR14R1
*
***********************************************************************
*  ZERO "CT" AREA                                                     *
***********************************************************************
         LA    R7,0(R5,R3)        LOAD TARGET  "CT" COLUMN ADDRESS
         LGR   R14,R7
*
         LGF   R0,VWMAXCOL        LOAD MAXIMUM "CT" COLUMN COUNT
         LGF   R15,VWMINCOL       LOAD MINIMUM "CT" COLUMN COUNT
         SGR   R0,R15             COMPUTE  NO.  OF  "CT" COLUMNS
         AGHI  R0,1
*
         STH   R0,EXNCOL-EXSORTLN(,R5) UPDATE NO OF "CT" COLUMNS
*
COPYZERO STH   R15,COLNO-COLEXTR(,R14)               RESET COLUMN NO
         MVC   COLDATA-COLEXTR(L'COLDATA,R14),ZACCUM RESET COLUMN VAL
*
         AGHI  R15,1              INCREMENT COLUMN#
         AGHI  R14,COLDATAL       ADVANCE   TO NEXT COLUMN
         AGHI  R3,COLDATAL        INCREMENT  RECORD LENGTH
         BRCT  R0,COPYZERO        LOOP THROUGH ALL COLUMNS
*
***********************************************************************
*  NORMALIZE EXISTING "CT" COLUMNS                                    *
***********************************************************************
         USING COLEXTR,R4
*
         LGH   R0,EXNCOL-EXSORTLN(,R2) LOAD ACTUAL "CT"  COLUMN COUNT
         LTGR  R0,R0
         BRNP  COPYLEN
*
COPYNORM LGH   R14,COLNO          LOAD      COLUMN NUMBER
         SGF   R14,VWMINCOL       COMPUTE   OFFSET TO INDICATED COLUMN
         LGHI  R15,COLDATAL
         MR    R14,R14
         LA    R1,0(R7,R15)
*
         MVC   COLDATA-COLEXTR(L'COLDATA,R1),COLDATA COPY  COLUMN  DATA
*
         AGHI  R4,COLDATAL        ADVANCE  TO NEXT COLUMN  OFFSET
         BRCT  R0,COPYNORM        LOOP THROUGH ALL COLUMNS IN EXTRACT
*
COPYLEN  STH   R3,WKOUTLEN
*
         STg   R5,savf4sag64rs2   cHANGE R2 -> EXPANDED RECORD
*
         LMG   R14,R12,savf4sag64rs14
         BR    R14
*
         DROP  R2
         DROP  R6
         DROP  R8
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        R E A D   N E X T   E N T I T Y   R E C O R D
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         sysstate amode64=NO
*****   NOTE this routine executes in AMODE 31 and must return via
*****   BSM 0,R10
*****
         USING FILEAREA,R6
*
READREC  CLI   FILEEOF,C'Y'       END-OF-FILE  -  ENTITY ???
         BRE   READEXIT           YES -  EXIT
*                                                                 @017I
         CLI   FILEETYP,C'S'      SAFR   PIPE ???                 @017I
         BRE   READPIPE           YES -  BRANCH                   @017I
*
*
         LG    R1,FILERECA        ADVANCE TO NEXT RECORD WITHIN BLOCK
         LGH   R0,FILERECL
         AGR   R1,R0
         CG    R1,FILEEODA
         BRNL  READBLK
*
         CLI   FILERECF,C'V'      VARIABLE LENGTH ???
         BRNE  READCNT1           NO  - BYPASS  SKIPPING OF "RDW"
         LGH   R0,0(,R1)
         AHI   R0,-4
         STH   R0,FILERECL        EXCLUDE/BYPASS  "RDW"  IF VARIABLE
         LA    R1,4(,R1)
*
READCNT1 agsi  FILEICNT,b001
*
         STG   R1,FILERECA
*
         J     READKXIT                                           @008C
*
                        EJECT
READBLK  llgt  R1,WKMR95WA
         USING THRDAREA,R1
*
         Llgt  R14,WKPLISTA       LOAD PARAMETER LIST ADDRESS
         llgt  R14,GPENVA-GENPARM(,R14)
         CLC   GPPHASE-GENENV(L'GPPHASE,R14),OP
         BRE   READUR20
*
         if (ltgf,r15,workazip,nz),and,    zIIP function available?    +
               (cli,thread_mode,eq,C'S')     and we are in SRB mode
*
           llgt R14,WKTOKNPA
           USING PARTAREA,R14
           agsi PARTMDSW,B001      INCREMENT SRB/TCB MODE SWITCH COUNT
           DROP R14
*
           lgr    r0,r13              save address of XRCK WA
           lgr    r13,r1              use thread work area
           la     r1,TCB_switch       Switch to TCB mode
           bassm  r14,r15             Call zIIP module
           lgr    r13,r0              restore XRCK WA
         endif
*
READUR20 LA    R1,UR20PARM
         llgf  R15,UR20ADDR
         MVC   UR20FC,H012        FUNCTION  = "LOCATE BLOCK"
         if clgf,R15,eq,GVBUR30        DB2 TABLE    ???
           MVC UR20FC,H008
         endif
*
         Bassm R14,R15
*
         LGH   R15,UR20RC
         LTR   R15,R15
         BRNZ  READEOF
*
         CLI   FILERECF,C'V'      VARIABLE LENGTH ???
         BRNE  READFIXD           NO  - BYPASS  SKIPPING OF "RDW"
*
         llgt  R1,UR20RECA
         STG   R1,FILEBLKA
*
         LGH   R0,0(,R1)          LOAD BLOCK  LENGTH FROM "BDW"
         AGR   R0,R1
         STG   R0,FILEEODA
         LGH   R0,4(,R1)          LOAD RECORD LENGTH FROM "RDW"
         AHI   R0,-4              EXCLUDE/BYPASS "RDW" IF VARIABLE
         STH   R0,FILERECL
         LA    R1,8(,R1)
         BRU   READCNT2
*
READFIXD llgt  R1,UR20RECA
         LGH   R0,UR20RECL
*
         STG   R1,FILEBLKA
         AGR   R0,R1
         STG   R0,FILEEODA
*
READCNT2 agsi  FILEICNT,b001
*
         STG   R1,FILERECA
*
         J     READKXIT           <---- Peter's change         @008C
*
READEOF  MVI   FILEEOF,C'Y'       BAD OPEN - TREAT AS END-OF-FILE
*
         MVC   UR20FC,H004        FUNCTION  = "CLOSE"  -  EVENT FILE
*
         LA    R1,UR20PARM
         llgf  R15,UR20ADDR
         if cij,r15,ne,0
           bassm R14,R15
         endif
*
READEOF5 ltgf  R14,FILEOPNA       REMOVE THIS FILE FROM OPEN LIST
         BRNP  READEXIT
*
         MVC   FILEOPNA,XHEXFF
*
         llgt  R0,WKOPNMAX        COMPUTE  SHIFT COUNT
         SGR   R0,R14
         AGHI  R0,-4
         BRNP  READEXIT
         SRL   R0,2
*
SHIFT    lgf   R1,4(,R14)
         ST    R1,0(,R14)
*
         ltgr  R1,R1
         BRNP  READEXIT
*
         ST    R14,FILEOPNA-FILEAREA(,R1)
         LA    R14,4(,R14)
         BRCT  R0,SHIFT
*
         BRU   READEXIT                                           @008I
*                                                                 @008I
READKXIT LT    R0,FILEKXEP        CALL "READ EXIT" IF SPECIFIED   @008I
         BRZ   READEXIT                                           @008I
*                                                                 @008I
         llgt  R14,FILEPRMA       RELATED  PARMTBL ENTRY          @008I
         LA    R1,PARMRXPM-PARMTBL(,R14)                          @008I
         ST    R1,WKCALLP3                                        @008I
*
         MVC   WKCALL64,FILERECA  Initialise Event Record address
         LAY   R1,WKCALL64        Load pointer to Event Record address
         STY   R1,WKCALLP4        Store pointer in parm list
         MVC   WKCALLPT,FILERECA     INITIALIZE RETURN REC ADDRESS@008I
*                                                                 @008I
         LH    R1,FILERECL        INITIALIZE RECORD LENGTH        @008I
         ST    R1,WKCALLRL                                        @008I
*                                                                 @008I
         LA    R1,FILERXAN        WORK AREA ANCHOR                @008I
         ST    R1,WKCALLP7                                        @008I
*                                                                 @008I
         LA    R1,FILERXRC        RETURN  CODE                    @008I
         ST    R1,WKCALLP8                                        @008I
*                                                                 @008I
         XC    FILERXRC,FILERXRC  ZERO RETURN CODE                @008I
*                                                                 @008I
         LA    R1,WKCALLPL                                        @008I
         llgf  R15,FILERXCA       (R0: TRUE ENTRY POINT)          @008I
         bassm R14,R15                                            @008I
*                                                                 @008I
         L     R15,FILERXRC       PROCESS RECORD ??               @008I
         Cije  R15,16,rtnabend    ABORT   ???                     @008I
         if Cij,R15,ge,8
           agsi FILEDCNT,b001     INCREMENT DUPLICATE COUNT       @008I
           BRU READREC                                            @008I
         endif
         MVC   FILERECA,WKCALLPT  RETURNED RECORD ADDRESS    @008I
         MVC   FILERXPT,WKCALLPT                                  @008I
         MVC   FILERXRL,WKCALLRL                                  @008I
*                                                                 @008I
readexit llgt  r1,wkmr95wa
         USING THRDAREA,R1
*
         llgt  R14,WKPLISTA       LOAD PARAMETER LIST ADDRESS
         llgt  R14,GPENVA-GENPARM(,R14)
         if CLC,GPPHASE-GENENV(L'GPPHASE,R14),eq,OP
           br  r10
         endif
*
         if (ltgf,r15,workazip,nz),and,    zIIP function available?    +
               (cli,thread_mode,eq,C'P')   and are we in TCB mode
*
           lgr    r0,r13              save address of XRCK WA
           lgr    r13,r1              use thread work area
           la     r1,SRB_switch       Switch to SRB mode
           bassm  r14,r15             Call zIIP module
           lgr    r13,r0              restore XRCK WA
         endif
*
         DROP R6
*
***********************************************************************
*  CONTINUE EXECUTING "GVBXRCK" MERGE LOGIC WHERE "READREC" WAS CALLED*
***********************************************************************
         br    r10
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * @017I
*                                                                 @017I
*        R E A D   "S A F R"   P I P E                            @017I
*                                                                 @017I
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * @017I
*                                                                 @017I
         USING FILEAREA,R6                                        @017I
*                                                                 @017I
READPIPE llgt  R1,WKPLISTA        ENVIRONMENT AREA ADDR           @017I
         MVC   WKPIPEEA,0(R1)                                     @017I
         LA    R0,FILEMAXL        BLOCK    LENGTH  ADDR           @017I
         ST    R0,WKPIPEBA                                        @017I
         LA    R0,FILEBLKA+4      BLOCK    POINTER ADDR           @017I
         ST    R0,WKPIPEPA                                        @017I
         LA    R0,FILERECF        BLOCK    FORMAT  ADDR           @017I
         ST    R0,WKPIPEFA                                        @017I
         LA    R0,FILERECL        "LRECL"          ADDR           @017I
         ST    R0,WKPIPELA                                        @017I
         LA    R0,WKPIPERC        RETURN   CODE    ADDR           @017I
         ST    R0,WKPIPERA                                        @017I
*                                                                 @017I
         LA    R1,WKPIPEPL        CALL "GENPIPE"                  @017I
         llgf  R15,GENPIPE                                        @017I
         Bassm R14,R15                                            @017I
*                                                                 @017I
         LGH   R15,WKPIPERC                                       @017I
         LTR   R15,R15                                            @017I
         BRZ   READPBLK                                           @017I
         cije  r15,8,readpeof
*                                                                 @017I
*        llgt  R14,0(,R5)       LOAD FILEAREA ADDR FROM OPEN LIST @017I
*        MVC   WKTXTLEN+2+36(8),UR20DDN-FILEAREA(R14)             @017I
         GVBMSG FORMAT,MSGNO=CK_GENPIPE_FAIL,SUBNO=2,                  +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(UR20DDN,l'UR20DDN),                               +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR                                           @017I
*                                                                 @017I
READPBLK LG    R14,FILEBLKA                                       @017I
*                                                                 @017I
         LGH   R15,FILEMAXL                                       @017I
         AR    R15,R14                                            @017I
         ST    R15,FILEEODA                                       @017I
*                                                                 @017I
         LH    R15,FILERECL                                       @017I
         CLI   FILERECF,C'V'                                      @017I
         BRNE  READPXIT                                           @017I
*                                                                 @017I
         LLGH  R0,0(,R14)                                         @017I
         STH   R0,FILEMAXL                                        @017I
*                                                                 @017I
         LGH   R15,4(,R14)           LOAD RECORD LENGTH FROM "RDW"@017I
         AHI   R15,-4                                             @017I
         AHI   R14,8                 ADVANCE BEYOND BDW/RDW       @017I
*                                                                 @017I
READPXIT STG   R14,FILERECA                                       @017I
         STH   R15,FILERECL                                       @017I
*                                                                 @017I
         br    r10                                                @017I
*                                                                 @017I
READPEOF MVI   FILEEOF,C'Y'          INDICATE END-OF-FILE         @017I
         BRU   READEOF5              ADJUST   OPEN   FILE  LIST   @017I
*                                                                 @017I
         DROP  R6                                                 @017I
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        L O A D   V I E W   D E F I N I T I O N   C H A I N          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
VIEWLOAD stmg  R14,R12,WKSAVSUB        SAVE CALLING SECTION'S REGISTERS
*
         OC    WKVDPDCB,WKVDPDCB       VDP  ALREADY LOADED ??
         BRNZ  VDPEXIT                 YES - DON'T  REPEAT
*
***********************************************************************
*  OPEN "MERGVDP" FILE                                                *
***********************************************************************
         LHI   R0,VDPFILEL        LOAD DCB  LENGTH
         GETMAIN RU,LV=(0),LOC=(BELOW) GET  MEMORY FOR DCB
         ST    R1,WKVDPDCB        SAVE DCB ADDRESS IN  CONTROL ELEMENT
         lgr   R2,R1
         USING IHADCB,R2
*
         LAY   R14,VDPFILE             COPY  DCB
         MVC   0(VDPFILEL,R2),0(R14)
*
         LAY   R14,VDPDCBE-VDPFILE(,R2)
         ST    R14,DCBDCBE
*
         llgt  R14,WKPLISTA       LOAD  START-UP  DATA   ADDRESS
         llgt  R14,GPSTARTA-GENPARM(,R14)
         USING STRTDATA,R14
         if CLC,SDDDNPFX,ne,BLANKS  CHECK IF DDNAME PREFIX SPECIFIED
           MVC DCBDDNAM-IHADCB(L'SDDDNPFX,R2),SDDDNPFX
         endif
         DROP  R14
*
         MVC   REENTWK(8),OPENPARM
         sysstate amode64=NO
         sam31
         OPEN  ((R2),(INPUT)),MODE=31,MF=(E,REENTWK)   OPEN  VIEW FILE
         sam64
         sysstate amode64=YES
         TM    48(R2),X'10'       SUCCESSFUL  ???
         BRO   VDPOPEN            NO  - PRINT ERROR    MESSAGE - STOP
*
         GVBMSG FORMAT,MSGNO=OPEN_VDP_FAIL,SUBNO=2,                    +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(DCBDDNAM,l'DCBDDNAM),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
         BRU   RTNERROR
*
VDPOPEN  LA    R7,WKVWLIST        INITIALIZE  PREVIOUS VIEW DEFN ADDR
*
         DROP  R2
*
***********************************************************************
*  READ VIEW DEFN PARAMETERS UNTIL END-OF-FILE                        *
***********************************************************************
VDPLOOP  llgt  R1,WKVDPDCB        LOAD VIEW  DEFINITION FILE DCB  ADDR
         sysstate amode64=NO
         sam31
         GET   (1)                READ NEXT  RECORD
         sam64
         sysstate amode64=YES
         llgtr R5,R1
*
         USING VDP0001_GENERATION_RECORD,R5
*
         LH    R0,VDP0001_RECORD_TYPE  LOAD RECORD TYPE CODE
*
***********************************************************************
*  PROCESS NEEDED RECORDS                                             *
***********************************************************************
         CHI   R0,0001            VDP GENERATION RECORD   ???
         BRE   VDP0001            YES -  EXTRACT SELECTED FIELDS
*
         CHI   R0,1000            VIEW    ???
         BRE   VDP1000            YES -   LOAD   IT
*
         CHI   R0,2000            COLUMN  ???
         BRE   VDP2000            YES -   LOAD   IT
*
         BRU   VDPLOOP            SKIP ALL OTHERS
*
         DROP  R5
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V D P   G E N E R A T I O N   R E C O R D                    *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING VDP0001_GENERATION_RECORD,R5
*
VDP0001  MVC   SVRUN#,VDP0001_RUN_NBR      SAVE RUN NUMBER
         MVC   SVRUNDT,VDP0001_RUN_DATE    SAVE RUN DATE
*
         BRU   VDPLOOP            READ NEXT "VDP"  RECORD
*
         DROP  R5
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V D P   V I E W   D E F I N I T I O N   R E C O R D          *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING VDP1000_VIEW_RECORD,R5
*
VDP1000  L     R0,WKVWCNT         INCREMENT  VIEW COUNT
         AHI   R0,1
         ST    R0,WKVWCNT
*
         LHI   R0,8+VWRECLEN      ALLOCATE   NEXT VIEW  DEFINITION REC
         GETMAIN RU,LV=(0),LOC=(ANY)
         MVC   0(8,R1),VIEWEYEB
         AHI   R1,8
         ST    R1,0(,R7)
*
         lgr   R0,R1              ZERO  VIEW DEFINITION AREA
         LHI   R1,VWRECLEN
         sgr   R14,R14
         SR    R15,R15
         MVCL  R0,R14
*
         llgt  R7,0(,R7)          INITIALIZE CURRENT CHAIN ELEMENT ADDR
         USING VIEWREC,R7
*
         XC    VWNEXT,VWNEXT      INITIALIZE FORWARD POINTER
*
***********************************************************************
*  COPY VIEW DEFINITION PARAMETERS INTO VIEW DEFN CHAIN               *
***********************************************************************
         MVC   VWVIEW#,VDP1000_VIEWID   VIEW ID
*
         LHI   R0,9999            MINIMUM  "CT" COLUMN NO.
         ST    R0,VWMINCOL
*
VDP1000X BRU   VDPLOOP            READ NEXT "VDP" RECORD
*
         DROP  R5
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        V D P   C O L U M N   D E F I N I T I O N   R E C O R D      *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING VDP2000_COLUMN_RECORD,R5
*
VDP2000  L     R0,VWCOLCNT        INCREMENT COLUMN COUNT
         AHI   R0,1
         ST    R0,VWCOLCNT
*
         CLI   VDP2000_EXTRACT_AREA_ID+3,CTAREA  "CT"  COLUMN   ???
         BRNE  VDP2000X           NO  - BYPASS MIN/MAX  RANGE
*
         LH    R0,VDP2000_COLUMN_ID+2   COLUMN NUMBER
*
         if C,R0,gt,VWMAXCOL      LARGER  THAN PREVIOUS MAXIMUM ???
           ST  R0,VWMAXCOL        YES - UPDATE
         endif
         if C,R0,lt,VWMINCOL      SMALLER THAN PREVIOUS MINIMUM ???
           ST  R0,VWMINCOL        YES - UPDATE
         endif
VDP2000X BRU   VDPLOOP
                        SPACE 3
VDPEOF   ds    0h
         sam64
         llgt  R2,WKVDPDCB
         sysstate amode64=NO
         sam31
         CLOSE ((R2)),MODE=31,MF=(E,REENTWK)
         sam64
         sysstate amode64=YES
*
VDPEXIT  lmg   R14,R12,WKSAVSUB        LOAD CALLING SECTION'S REGISTERS
         BR    R14
*
         DROP  R5
         DROP  R7
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*    L O C A T E   M A T C H I N G   V I E W   D E F I N I T I O N    *
*                                                                     *
*   (MATCHING VIEW DEFINITION ADDRESS IS SAVED IF VIEW HAS "CT" COLS) *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         USING FILEAREA,R6
*
LOCVIEW  Lg    R14,FILERECA       LOAD  VIEW#
         L     R0,EXVIEW#-EXSORTLN(,R14)
         SRL   R0,1
*
         llgt  R14,WKVWLIST       LOAD  FIRST  VIEW ADDRESS
         USING VIEWREC,R14
*
LOCVSRCH ltgr  R14,R14            END-OF-LIST   ???
         BRNP  LOCVNOT            YES - REQUEST NOT FOUND
         C     R0,VWVIEW#         MATCHING REQUEST NUMBERS ???
         BRE   LOCVSAVE           YES - PERFORM INITIALIZATION
         llgt  R14,VWNEXT         LOAD  ADDRESS OF NEXT LIST ENTRY
         BRU   LOCVSRCH           LOOP  THROUGH ENTIRE  LIST
*
LOCVNOT  DS    0H                 DISPLAY "UNABLE TO FIND VIEW" MESSAGE
         CVD   R0,WKDBLWRK
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         UNPK  WKDBLWK2,WKDBLWRK
         GVBMSG FORMAT,MSGNO=CK_VIEW_NF,SUBNO=2,                       +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(WKDBLWK2,l'WKDBLWK2),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
         BRU   RTNERROR
*
LOCVSAVE ltgf  R15,VWMAXCOL       ANY  "CT" COLUMNS ???
         BRNP  LOCVEXIT
*
         ST    R14,FILEVREC       SAVE CURRENT VIEW DEFINITION ADDRESS
*
LOCVEXIT BR    R10
*
         DROP  R6
         DROP  R14
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*                   C A L L   U S E R   E X I T                       *
*                                                                     *
*       PARAMETERS:                                                   *
*         R0:  TRUE ENTRY POINT ADDRESS                               *
*         R1:  PARAMETER  LIST  ADDRESS                               *
*         R14: RETURN ADDRESS                                         *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
CALLEXIT llgt  R8,WKMR95WA        LOAD "GVBMR95" WORK AREA ADDR
         stg   r8,callexit_r8save
         llgt  R8,CEEWADDR-THRDAREA(,R8) COMMON  LANG INTERFACE AREA
         USING LEINTER,R8
*
         ST    R0,LESUBADR        SAVE TRUE ENTRY POINT ADDRESS
*
         ST    R1,LEPARMP         PASS TRUE  PARAMETER  LIST ADDRESS
         ST    R14,LERETADR       SAVE RETURN  ADDRESS
*
         LA    R1,LEPARM
         l     R15,LECEEADR       CALL COMMON LANG INTERFACE
         BASR  R14,R15
*
         llgt  R14,LERETADR       LOAD RETURN ADDR
         lgf   R15,LERTNC         LOAD RETURN CODE
         lg    r8,callexit_r8save
         BR    R14                RETURN
*
         DROP  R8
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C O N V E R T   T I M E   T O   H H M M S S H H              *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
CONVHMS  SRDL  R0,32              DIVIDE  AND    GET  HOURS
         D     R0,F360K
         CVD   R1,WKDBLWRK        CONVERT HOURS   TO  DECIMAL
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         UNPK  WKDBLWK2+0(2),WKDBLWRK
*
         SRDL  R0,32              DIVIDE  AND    GET  MINUTES
         D     R0,F6000
         CVD   R1,WKDBLWRK        CONVERT MINUTES TO  DECIMAL
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         UNPK  WKDBLWK2+2(2),WKDBLWRK
*
         SRDL  R0,32              DIVIDE  AND    GET  SECONDS
         D     R0,F310
         CVD   R1,WKDBLWRK        CONVERT SECONDS TO  DECIMAL
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         UNPK  WKDBLWK2+4(2),WKDBLWRK
*
         CVD   R0,WKDBLWRK        CONVERT HUNDREDTHS  TO DECIMAL
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         UNPK  WKDBLWK2+6(2),WKDBLWRK
*
         PACK  WKDBLWRK,WKDBLWK2  CONVERT HHMMSSHH    TO BINARY
         CVB   R0,WKDBLWRK
*
         BR    R14                RETURN
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*   D Y N A M I C A L L Y   A L L O C A T E   E N T I T Y   F I L E   *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         USING FILEAREA,R6
         USING PARMTBL,R3
*
         sysstate amode64=NO
DYNALLOC stmg  R14,R12,WKSAVSUB         SAVE  MAINLINE  REGISTERS
*
*
*
***********************************************************************
*  CHECK IF DDNAME EXPLICITLY INCLUDED IN JCL (SCAN "TIOT")           *
***********************************************************************
         llgt  R14,WKTIOTA        LOAD TIOT  ADDRESS
         LA    R14,24(,R14)       LOAD ADDRESS OF FIRST DDNAME
         SR    R15,R15            CLEAR LENGTH REGISTER
DYNATIOT CLC   4(8,R14),UR20DDN   MATCHING  DDNAME  ???
         BRE   DYNAEXIT           YES - RETURN  (DDNAME IS  IN  JCL)
*
         IC    R15,0(R14)         LOAD  ENTRY LENGTH
         AR    R14,R15            ADVANCE  TO NEXT    DDNAME
         ICM   R1,B'1111',0(R14)  IS  LENGTH  OF NEXT ENTRY ZERO ???
         BRNZ  DYNATIOT           NO  - LOOP  THROUGH TIOT
*
*
***********************************************************************
*  IF DDNAME NOT IN JCL, GET "TEMPLATE" DATASET NAME FROM "PARMDSN"   *
***********************************************************************
         LAY   R9,WKDYNAR         DYNAMIC  ALLOCATION PARAMETERS
         USING M35SVC99,R9
*
         LA    R0,M35SVC99        CLEAR UR35  PARM AREA
         LA    R1,M35S99LN
         SGR   r14,r14
         sgr   R15,r15
         MVCL  R0,R14
*
*
***********************************************************************
* REMOVE RELATIVE GENERATION NUMBER FROM END OF DATASET NAME IF THERE *
***********************************************************************
DYNADSN  LA    R0,L'PARMDSN       LOAD MAXIMUM LENGTH OF DSNAME
         LA    R14,PARMDSN+L'PARMDSN   POINT TO LAST BYTE OF NAME
DYNALOOP bctgr R14,0              BACKUP TO PREVIOUS BYTE
         CLI   0(R14),C' '        TRAILING BLANK  ???
         BRNE  DYNAGDG            NO  - END FOUND
         BRCT  R0,DYNALOOP
*
         BRU   DYNAEXIT           RETURN - DSNAME NOT AVAILABLE
*
DYNAGDG  CLI   0(R14),C')'        GENERATION SPECIFIED ???
         BRNE  DYNAGDG6           NO  - BYPASS  SCAN   FOR  LEFT  PAREN
*
         lgr   R15,R14            SAVE  ")" ADDRESS
         cijl  r0,4,dynaexit      CHECK LENGTH  AGAINST  MINIMUM
*
DYNAGDG2 bctgr R14,0              BACKUP TO PREVIOUS BYTE
         CLI   0(R14),C'('        LEFT  PARENTHESIS  ???
         BRE   DYNAGDG4           YES - BEGINNING FOUND
         BRCT  R0,DYNAGDG2        LOOP
*
         BRU   DYNAEXIT           RETURN - DSNAME NOT AVAILABLE
*
DYNAGDG4 sgr   R15,R14            COMPUTE  LENGTH (+1)
         AHI   R15,-1             EXCLUDE "("
         BRNP  DYNAEXIT
         BCTR  R15,0              DECREMENT FOR "EX"
         exrl  R15,DYNAGMVC       MOVE    RELATIVE GENERATION
*
         bctgr R14,0              EXCLUDE "("
*
*
***********************************************************************
*  COPY DATASET NAME INTO DYNAMIC ALLOCATION PARAMETER AREA           *
***********************************************************************
DYNAGDG6 LA    R0,PARMDSN         COMPUTE DSNAME LENGTH (-1)
         sgr   R14,R0
         MVI   M35DSNAM,C''''     LEADING  QUOTE
         exrl  R14,DYNADMVC       COPY    DSNAME
         LA    R15,M35DSNAM+2(R14)
         MVI   0(R15),C''''       TRAILING QUOTE
*
         MVC   M35DDNAM,UR20DDN   ALLOCATE CORRECT DDNAME
         MVC   M35STATS,SHR       STATUS DISPOSITION
         MVI   M35CLOSE,C'Y'      DEALLOCATE AT CLOSE
         MVI   M35FCODE,C'1'      INDICATE ALLOCATION
*
         XC    M35MEMBR,M35MEMBR  FORCE  GENERATION NUMBER TO "0"
         CLC   PARMGDG,BLANKS     DEFAULT GDG =  SPECIFIED ???
         BRE   DYNASCAN           NO  -   BYPASS DEFAULT   LOGIC
         CLI   PARMGDG,C' '       LEADING BLANK  ???
         BRNE  DYNAGDG8
         MVC   M35MEMBR(1),PARMGDG+1
         BRU   DYNASCAN
DYNAGDG8 MVC   M35MEMBR(2),PARMGDG+0
*
*
***********************************************************************
*  INSERT CORRECT ENTITY DSN NODE NAME INTO TEMPLATE                  *
***********************************************************************
DYNASCAN LA    R0,1(,R14)        LOAD   DSNAME LENGTH
         LA    R1,M35DSNAM+1     LOAD   DSNAME ADDRESS
DYNAINS  ds    0h
         if CLI,0(R1),eq,C'%'    RECORD SOURCE INSERT ???
           MVC 0(1,R1),FILERSRC
         endif

         if Cij,R0,gt,1,and,     LAST   BYTE (NOT ENOUGH ROOM FOR !!)  +
               CLC,=CL2'!!',eq,0(R1)    PARTITION
           MVC 0(2,R1),FILEPART
         endif
*
         LA    R1,1(,R1)
         BRCT  R0,DYNAINS
*
DYNARTRY LA    R2,2              SET DYNACALL LOOP CTR TO 2
*
DYNACALL LA    R1,M35SVC99        --> CALLING STRUCTURE
         ST    R1,WKDBLWRK
         OI    WKDBLWRK,X'80'
         LA    R1,WKDBLWRK
*
         llgf  R15,GVBUR35        GO DO THE ALLOCATION
         bassm R14,R15
*
         LTR   R15,R15            SUCCESSFUL  ???
         BRZ   DYNAEXIT           YES - EXIT
*
         LGR   R4,r15            Save return code
*
         LLGT  R14,WKPLISTA        PARAMETER LIST ADDRESS
         Llgt  R14,GPENVA-GENPARM(,R14)
         GVBMSG LOG,MSGNO=DYNALLOC_RETRY,SUBNO=2,                      +
               GENENV=(R14),                                           +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(M35DSNAM,l'M35DSNAM),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)

         LA    R1,DYNWAIT        POINT TO 4000 .01 SECONDS
*
         STIMER WAIT,BINTVL=(1)  WAIT 40 SECONDS AND RETRY DYNACALL
*
         BRCT  R2,DYNACALL       LOOP UNTIL R2 CTR = 0
*
*
         CVD   R4,WKDBLWRK
         OI    WKDBLWRK+L'WKDBLWRK-1,X'0F'
         UNPK  WKDBLWK2(3),WKDBLWRK
*
         LLGT  R14,WKPLISTA        PARAMETER LIST ADDRESS
         Llgt  R14,GPENVA-GENPARM(,R14)
         GVBMSG LOG,MSGNO=DYNALLOC_UR35_FAIL,SUBNO=3,                  +
               GENENV=(R14),                                           +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(M35DSNAM,l'M35DSNAM),                             +
               SUB3=(WKDBLWK2,3),                                      +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
DYNAEXIT lmg   R14,R12,WKSAVSUB         RESTORE REGISTERS
         bsm   0,r14                    RETURN
*
         ds    0d
DYNADMVC MVC   M35DSNAM+1(0),PARMDSN  * *  E X E C U T E D
         ds    0d
DYNAGMVC MVC   M35MEMBR(0),1(R14)   * * *  E X E C U T E D
*
         DROP  R3
         DROP  R6
         DROP  R9
                        EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        C L O S E   T H E   E N T I T Y   F I L E S                  *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
CLOSFILE stmg  R14,R12,WKSAVSUB         SAVE  MAINLINE  REGISTERS
*
*        this routine receives control in 31 bit mode
*
*
***********************************************************************
*  SINGLE THREAD PARTITION RECORD COUNTS                              *
***********************************************************************
         MVC   REENTWK(MDLENQPL),MDLENQP
         enq   MF=(E,REENTWK)
         LTR   R15,R15
         BRZ   CLOSDONE
*
         GVBMSG FORMAT,MSGNO=ENQ_FAIL,SUBNO=3,                         +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(LPGMNAME,l'LPGMNAME),                             +
               SUB3=(CLOSETEXT,l'CLOSETEXT),                           +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
         BRU   RTNERROR
*
*
***********************************************************************
*  OPEN CONTROL REPORT FILE  AND  PRINT REPORT HEADINGS               *
***********************************************************************
CLOSDONE llgt  R3,WKGLOBA         INCREMENT PARTITIONS DONE COUNT
         USING GLOBAREA,R3
*
         LH    R15,GLOBDONE
         LA    R15,1(,R15)
         STH   R15,GLOBDONE
*
*        LHI   R15,WORKLEN-1
*        AR    R15,R13
*          ly  r2,SNAPDCBA
*        SNAP  DCB=(r2),PDATA=REGS,ID=200,STORAGE=((R13),(R15))
*
CLOSOPEN ds    0h
*
         llgt  R2,WKRPTDCB        get the dcb addr
         USING IHADCB,R2
*
         MVC   REENTWK(8),OPENPARM
         OPEN  ((R2),OUTPUT),MODE=31,MF=(E,REENTWK) OPEN  FILE
         if tm,dcboflgs,dcbofopn,no  did it fail to open?
*
           GVBMSG FORMAT,MSGNO=OPEN_MERGRPT_FAIL,SUBNO=2,              +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(DCBDDNAM,l'DCBDDNAM),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
           BRU closdeq
         endif
*
*
CLOSRPT  llgt  R6,WKFILBEG        FIRST FILE CONTROL AREA
         USING FILEAREA,R6
*
         lay   R14,headerd
         MVC   WKPRTTXT,0(R14)
         MVI   WKPRTTXT,C'1'
         PUT   (R2),WKPRTTXT
*
         lay   R14,header09       PARTITION RECAP
         MVC   WKPRTTXT,0(R14)
         MVC   WKPRTTXT+11(2),FILEPART
         PUT   (R2),WKPRTTXT
*
         lay   R0,header10        COLUMN HEADINGS
         PUT   (R2),(R0)
         lay   R0,header11        COLUMN DASHES
         PUT   (R2),(R0)
*
         lay   R14,header12
         MVC   WKPRTTXT,0(R14)
*
CLOSLOOP cgf   R6,WKFILMAX        ALL  FILES CLOSED    ???
         BRNL  CLOSEXIT
*
         LT    R0,FILEKXEP        KEY   EXIT SPECIFIED ???        @008I
         JNZ   CLOSRXIT                                           @008C
         LT    R0,FILERXEP        READ  EXIT SPECIFIED ???        @008C
         JZ    CLOSCLOS                                           @008C
*
CLOSRXIT DS    0H                                                 @008I
         LA    R1,FILERXAN        WORK  AREA    ANCHOR ADDRESS    @008C
         lt    R15,0(,R1)                                         @008C
         JZ    CLOSCLOS                                           @008C
*
         ST    R1,WKCALLP7
         LA    R1,WKEXITRC        RETURN  CODE
         ST    R1,WKCALLP8
*
         llgt  R14,FILEPRMA       RELATED PARMTBL ENTRY
         LA    R1,PARMRXPM-PARMTBL(,R14)
         ST    R1,WKCALLP3
*
         XC    WKCALL64,WKCALL64  Null 64-bit Event Record address
         LAY   R1,WKCALL64        Load pointer to Event Record address
         STY   R1,WKCALLP4        Store pointer in parm list
         XC    WKEXITRC,WKEXITRC  ZERO  PREVIOUS RETURN CODE
*
         llgt  R14,WKPLISTA       LOAD PARAMETER LIST ADDRESS
         llgt  R14,GPENVA-GENPARM(,R14)
         MVC   WKCALLEV,0(R14)
         MVC   WKCALLEV+GPPHASE-GENENV(L'GPPHASE),CL
         LA    R14,WKCALLEV
         ST    R14,WKCALLP1
*
         LA    R1,WKCALLPL        CALL  READ     EXIT
         llgf  R15,FILERXCA
         bassm R14,R15
*
CLOSCLOS MVC   UR20FC,H004        FUNCTION  = "CLOSE"  -  EVENT FILE
*
         LA    R1,UR20PARM
         llgf  R15,UR20ADDR
         if cij,r15,ne,0
           bassm R14,R15
         endif
*
CLOSCNT  oc    FILEICNT,fileicnt  ANY RECORDS READ FROM THIS FILE ???
         jz    CLOSNEXT           NO  - BYPASS PRINTING
*
         lg    r0,fileicnt        Get binary count
         cvdg  r0,wkdblwk2        convert to decimal
         AP    WKPRTCNT,wkdblwk3+2(6) ACCUM TOTAL PARTITION RECORDS
         AP    GLOBREAD,wkdblwk3+2(6) accUM TOTAL RECORDS   READ
*
         LH    R0,FILERECL
         CVD   R0,WKDBLWRK
*
* RTC19034 WKDBLWK2 replaced by WKFILCT PL12 to avoid overflow in MP
*          when > 1B records present this will support a max byte
*          count of 999,999,999,999  Allows 1B recs of 500LRECL
*
         ZAP   WKFILCT,wkdblwk3+2(6)  RTC19034
         MP    WKFILCT,WKDBLWRK+5(3)  RTC19034
         AP    WKPRTBYT,WKFILCT+4(8) ACCUM TOTAL PRTN BYTES RTC19034
         AP    GLOBBYTE,WKFILCT+4(8) ACCUM TOTAL BYTES READ RTC19034
*
         MVC   STATDDN,UR20DDN
         MVC   STATRTYP+0(8),FILERTYP
         MVI   STATRTYP+8,C'/'
         MVC   STATRTYP+9(1),FILERSRC
*
         MVC   STATMSTI,COUNTMSK
         ED    STATMSTI,wkdblwk3+2
         OI    STATMSTI+L'STATMSTI-1,X'F0'
*
         MVC   STATMSTL,LENGMSK
         ED    STATMSTL,WKDBLWRK+5
         OI    STATMSTL+L'STATMSTL-1,X'F0'
*
         MVC   STATMSTB,BYTEMSK
         ED    STATMSTB,WKFILCT+4     RTC19034
         OI    STATMSTB+L'STATMSTB-1,X'F0'
*
         lg    r0,filedcnt        Get binary count
         cvdg  r0,wkdblwk2        convert to decimal
         AP    WKPRTDUP,wkdblwk3+2(6)
         AP    GLOBDUPE,wkdblwk3+2(6)
*
         MVC   STATMSTD,COUNTMSK
         ED    STATMSTD,wkdblwk3+2
         OI    STATMSTD+L'STATMSTD-1,X'F0'
         PUT   (R2),WKPRTTXT
*
CLOSNEXT LA    R6,FILELEN(,R6)
         BRU   CLOSLOOP
*
CLOSEXIT lg    r0,wkgencnt        Get binary count
         cvd   r0,wkdblwk2        convert to decimal
         AP    WKPRTCNT,wkdblwk2+2(6) ACCUM  TOTAL PARTITION RECORDS
         AP    GLOBREAD,wkdblwk2+2(6) ACCUM  TOTAL RECORDS   READ
*
         AP    WKPRTBYT,WKGENBYT  ACCUMULATE TOTAL PARTITION BYTES
         AP    GLOBBYTE,WKGENBYT  ACCUMULATE TOTAL BYTES     READ
*
         MVC   STATDDN(19),GENERATE
*
         MVC   STATMSTI,COUNTMSK
         ED    STATMSTI,wkdblwk2+2
         OI    STATMSTI+L'STATMSTI-1,X'F0'
*
         MVC   STATMSTL,BLANKS
*
         MVC   STATMSTB,BYTEMSK
         ED    STATMSTB,WKGENBYT
         OI    STATMSTB+L'STATMSTB-1,X'F0'
*
         MVC   STATMSTD,BLANKS
*
         PUT   (R2),WKPRTTXT
*
         lay   R14,header14
         MVC   WKPRTTXT,0(R14)
         MVC   STATMSTI,COUNTMSK
         ED    STATMSTI,WKPRTCNT
         OI    STATMSTI+L'STATMSTI-1,X'F0'
         MVC   STATMSTB,BYTEMSK
         ED    STATMSTB,WKPRTBYT
         OI    STATMSTB+L'STATMSTB-1,X'F0'
         MVC   STATMSTD,COUNTMSK
         ED    STATMSTD,WKPRTDUP
         OI    STATMSTD+L'STATMSTD-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         lay   R0,headerb
         PUT   (R2),(R0)
         lay   R0,headerd
         PUT   (R2),(R0)
*
         llgt  R5,WKTOKNPA
         USING PARTAREA,R5
*
         MVI   WKPRTTXT+0,C' '
         MVC   WKPRTTXT+1(L'WKPRTTXT-1),WKPRTTXT+0
*
         lg    r14,PARTFNDE
         cvd   r14,wkdblwrk
         AP    GLOBFNDE,wkdblwrk+2(6)
         MVC   WKPRTTXT+00(22),=CL22' (E) LOOKUPS FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),wkdblwrk+2
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         lg    r14,PARTNOTE
         cvd   r14,wkdblwrk
         AP    GLOBNOTE,wkdblwrk+2(6)
         MVC   WKPRTTXT+01(21),=CL21'(E) LOOKUPS NOT FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),wkdblwrk+2
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         lg    r14,PARTFNDT
         cvd   r14,wkdblwrk
         AP    GLOBFNDT,wkdblwrk+2(6)
         MVC   WKPRTTXT+01(21),=CL21'(T) LOOKUPS FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),wkdblwrk+2
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         lg    r14,PARTNOTT
         cvd   r14,wkdblwrk
         AP    GLOBNOTT,wkdblwrk+2(6)
         MVC   WKPRTTXT+01(21),=CL21'(T) LOOKUPS NOT FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),wkdblwrk+2
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         lg    r14,PARTFNDX
         cvd   r14,wkdblwrk
         AP    GLOBFNDX,wkdblwrk+2(6)
         MVC   WKPRTTXT+01(21),=CL21'(X) LOOKUPS FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),wkdblwrk+2
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         lg    r14,PARTNOTX
         cvd   r14,wkdblwrk
         AP    GLOBNOTX,wkdblwrk+2(6)
         MVC   WKPRTTXT+01(21),=CL21'(X) LOOKUPS NOT FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),wkdblwrk+2
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         lg    r14,PARTFNDR
         cvd   r14,wkdblwrk
         AP    GLOBFNDR,wkdblwrk+2(6)
         MVC   WKPRTTXT+01(21),=CL21'(R) LOOKUPS FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),wkdblwrk+2
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         lg    r14,PARTNOTR
         cvd   r14,wkdblwrk
         AP    GLOBNOTR,wkdblwrk+2(6)
         MVC   WKPRTTXT+01(21),=CL21'(R) LOOKUPS NOT FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),wkdblwrk+2
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         lg    r14,PARTFNDB
         cvd   r14,wkdblwrk
         AP    GLOBFNDB,wkdblwrk+2(6)
         MVC   WKPRTTXT+01(21),=CL21'(B) LOOKUPS FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),wkdblwrk+2
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         lg    r14,PARTNOTB
         cvd   r14,wkdblwrk
         AP    GLOBNOTB,wkdblwrk+2(6)
         MVC   WKPRTTXT+01(21),=CL21'(B) LOOKUPS NOT FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),wkdblwrk+2
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*                                                                 @020I
         lay   R0,headerb                                         @020I
         PUT   (R2),(R0)                                          @020I
*
         MVI   WKPRTTXT+0,C' '
         MVC   WKPRTTXT+1(L'WKPRTTXT-1),WKPRTTXT+0
*
         MVC   WKPRTTXT+01(29),=CL29'BUFFER USE: XXXXXXXXXXXXXXX  '
         MVC   WKPRTTXT+30(27),=CL27'BUFFER MAX: XXXXXXXXXXXXXXX'
         if Ltg,R0,WKBUFHI,p
           SG  R0,WKBUFBEG
         endif
         CVD   R0,WKDBLWRK
         MVC   WKPRTTXT+13(15),COUNTMSK
         ED    WKPRTTXT+13(15),WKDBLWRK+2
         OI    WKPRTTXT+13+L'COUNTMSK-1,X'F0'
         LG    R0,WKBUFMAX
         SG    R0,WKBUFBEG
         CVD   R0,WKDBLWRK
         MVC   WKPRTTXT+42(15),COUNTMSK
         ED    WKPRTTXT+42(15),WKDBLWRK+2
         OI    WKPRTTXT+42+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         lay   R0,headerb
         PUT   (R2),(R0)
*
         MVI   WKPRTTXT+0,C' '
         MVC   WKPRTTXT+1(L'WKPRTTXT-1),WKPRTTXT+0
*
         lg    r14,PARTBRKC
         cvd   r14,wkdblwrk
         AP    GLOBBRKC,wkdblwrk+2(6)
         MVC   WKPRTTXT+00(22),=CL22' BREAK   KEY COUNT'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),wkdblwrk+2
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         lg    r14,PARTMDSW
         cvd   r14,wkdblwrk
         AP    GLOBMDSW,wkdblwrk+2(6)
         MVC   WKPRTTXT+00(22),=CL22' SRB/TCB SW  COUNT'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),wkdblwrk+2
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         lay   R0,headerb
         PUT   (R2),(R0)
         lay   R0,headerd
         PUT   (R2),(R0)
*
*  PRINT CUMULATIVE COUNTS
*
CLOS_100 DS    0H
         CLC   GLOBDONE,GLOBPART  LAST PARTITION TO FINISH ???
         BRL   CLOSfin            NO - WAIT   TO PRINT
*
         lay   R14,headerd
         MVC   WKPRTTXT,0(R14)
         MVI   WKPRTTXT,C'1'
         PUT   (R2),WKPRTTXT
         lay   R0,header16
         PUT   (R2),(R0)
         lay   R0,headerb
         PUT   (R2),(R0)
         lay   R0,header17
         PUT   (R2),(R0)
         lay   R0,header18
         PUT   (R2),(R0)
*
         lay   R14,header14
         MVC   WKPRTTXT,0(R14)
*
         MVC   WKPRTTXT(20),=CL20' '
         MVC   STATMSTI,COUNTMSK
         ED    STATMSTI,GLOBREAD
         OI    STATMSTI+L'STATMSTI-1,X'F0'
         MVC   STATMSTB,BYTEMSK
         ED    STATMSTB,GLOBBYTE
         OI    STATMSTB+L'STATMSTB-1,X'F0'
         MVC   STATMSTD,COUNTMSK
         ED    STATMSTD,GLOBDUPE
         OI    STATMSTD+L'STATMSTD-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         MVI   WKPRTTXT+0,C' '
         MVC   WKPRTTXT+1(L'WKPRTTXT-1),WKPRTTXT+0
*
         MVC   WKPRTTXT+00(22),=CL22'0(E) LOOKUPS FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),GLOBFNDE
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         MVC   WKPRTTXT+00(22),=CL22' (E) LOOKUPS NOT FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),GLOBNOTE
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         MVC   WKPRTTXT+01(21),=CL21'(T) LOOKUPS FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),GLOBFNDT
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         MVC   WKPRTTXT+01(21),=CL21'(T) LOOKUPS NOT FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),GLOBNOTT
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         MVC   WKPRTTXT+01(21),=CL21'(X) LOOKUPS FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),GLOBFNDX
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         MVC   WKPRTTXT+01(21),=CL21'(X) LOOKUPS NOT FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),GLOBNOTX
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         MVC   WKPRTTXT+01(21),=CL21'(R) LOOKUPS FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),GLOBFNDR
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         MVC   WKPRTTXT+01(21),=CL21'(R) LOOKUPS NOT FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),GLOBNOTR
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         MVC   WKPRTTXT+01(21),=CL21'(B) LOOKUPS FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),GLOBFNDB
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         MVC   WKPRTTXT+01(21),=CL21'(B) LOOKUPS NOT FOUND'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),GLOBNOTB
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         lay   R0,headerb                                         @020I
         PUT   (R2),(R0)                                          @020I
*                                                                 @020I
         MVC   WKPRTTXT+01(29),=CL29'EXT    USE: XXXXXXXXXXXXXXX  '
         MVC   WKPRTTXT+30(27),=CL27'EXT    MAX: XXXXXXXXXXXXXXX'
         LG    R0,GLOBXHI
         CVD   R0,WKDBLWRK
         MVC   WKPRTTXT+13(15),COUNTMSK
         ED    WKPRTTXT+13(15),WKDBLWRK+2
         OI    WKPRTTXT+13+L'COUNTMSK-1,X'F0'
         LH    R0,GLOBtSIZ                                        @020C
         SLL   R0,20                                              @020C
         CVD   R0,WKDBLWRK
         MVC   WKPRTTXT+42(15),COUNTMSK
         ED    WKPRTTXT+42(15),WKDBLWRK+2
         OI    WKPRTTXT+42+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         MVC   WKPRTTXT+01(29),=CL29'EXT  COUNT: XXXXXXXXXXXXXXX  ' 20I
         MVC   WKPRTTXT+30(27),=CL27'EXT  QUEUE: XXXXXXXXXXXXXXX' @020I
         L     R0,GLOBXCNT                                        @020I
         CVD   R0,WKDBLWRK                                        @020I
         MVC   WKPRTTXT+13(15),COUNTMSK                           @020I
         ED    WKPRTTXT+13(15),WKDBLWRK+2                         @020I
         OI    WKPRTTXT+13+L'COUNTMSK-1,X'F0'                     @020I
         LH    R0,GLOBXQUM                                        @020I
         CVD   R0,WKDBLWRK                                        @020I
         MVC   WKPRTTXT+42(15),COUNTMSK                           @020I
         ED    WKPRTTXT+42(15),WKDBLWRK+2                         @020I
         OI    WKPRTTXT+42+L'COUNTMSK-1,X'F0'                     @020I
         PUT   (R2),WKPRTTXT                                      @020I
*                                                                 @020I
         lay   R0,headerb
         PUT   (R2),(R0)
*
         MVI   WKPRTTXT+0,C' '
         MVC   WKPRTTXT+1(L'WKPRTTXT-1),WKPRTTXT+0
*
         MVC   WKPRTTXT+00(22),=CL22' BREAK   KEY COUNT'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),GLOBBRKC
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         MVC   WKPRTTXT+00(22),=CL22' SRB/TCB SW  COUNT'
         MVC   WKPRTTXT+23(15),COUNTMSK
         ED    WKPRTTXT+23(15),GLOBMDSW
         OI    WKPRTTXT+23+L'COUNTMSK-1,X'F0'
         PUT   (R2),WKPRTTXT
*
         lay   R0,headerb
         PUT   (R2),(R0)
         lay   R0,headerd
         PUT   (R2),(R0)
*
*        only last task gets here, and at this point it is done with
*        the tokens.
*        So clean up the all the non-task tokens now
*
         MVC   WKTOKNAM+0(8),GENEVA     set up the global token name
         MVC   WKTOKNAM+8(8),TKNNAME
*
         llgt  R9,WKPLISTA
         USING GENPARM,R9
         llgt  R14,GPSTARTA       LOAD  START-UP  DATA   ADDRESS
         USING STRTDATA,R14
         if CLC,SDDDNPFX,ne,BLANKS    CHECK IF DDNAME PREFIX SPECIFIED
           MVC WKTOKNAM+8(4),SDDDNPFX
         endif
         DROP  R14,r9
*
         CALL  IEANTRT,(TOKNLVL2,WKTOKNAM,WKTOKN,WKTOKNRC),            X
               MF=(E,REENTWK)
         if (LT,r15,wktoknrc,z)   retrieve successful
           llgt r1,wktokn         get global area address
           ahi  r1,-8               move it back 8 bytes
           STORAGE RELEASE,ADDR=(1),LENGTH=GLOBLEN+8  GIVE STORAGE BACK
*          assume that this works as expected?

           llgt r15,=v(iean4dl)   get address without bit 32
           CALL (15),(TOKNLVL2,WKTOKNAM,WKTOKNRC),MF=(E,REENTWK)
*          this should delete the level 2 token
         endif
*
CLOSfin  ds   0h
         MVC   REENTWK(8),OPENPARM
         CLOSE ((R2)),MODE=31,MF=(E,REENTWK)     CLOSE     FILE
*
CLOSDEQ  lgf   R0,WKFILSIZ              RELEASE FILE AREAS
         llgt  R1,WKFILBEG
         FREEMAIN RU,A=(1),LV=(0)
*
         MVC   REENTWK(MDLDEQPL),MDLDEQP
         DEQ   MF=(E,REENTWK)
*
         lmg   R14,R12,WKSAVSUB         RESTORE REGISTERS
         bsm   0,r14                    RETURN
*
*   This section prints the control input before we start
*   processing
*
print_input stmg  R14,R12,WKSAVSUB      SAVE  MAINLINE  REGISTERS
*
*        this routine receives control in 31 bit mode
*
         MVC   REENTWK(MDLENQPL),MDLENQP
         enq   MF=(E,REENTWK)
         if LTR,R15,R15,nz
*
           GVBMSG FORMAT,MSGNO=ENQ_FAIL,SUBNO=3,                       +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(LPGMNAME,l'LPGMNAME),                             +
               SUB3=(InitPTEXT,l'InitPTEXT),  print input              +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
           BRU RTNERROR
         endif
*
***********************************************************************
*  OPEN CONTROL REPORT FILE  AND  PRINT REPORT HEADINGS               *
***********************************************************************
         llgt  R3,WKGLOBA         INCREMENT PARTITIONS DONE COUNT
         USING GLOBAREA,R3
*
         lghi  R0,RPTFILEL        LOAD DCB  LENGTH
         GETMAIN RU,LV=(0),LOC=(BELOW) GET  MEMORY FOR DCB
*
         ST    R1,WKRPTDCB        SAVE DCB ADDRESS IN  CONTROL ELEMENT
         LGR   R2,R1
         USING IHADCB,R2
*
         LAY   R14,RPTFILE             COPY  DCB
         MVC   ihadcb(RPTFILEL),0(R14)
*
         LAY   R14,RPTDCBE-RPTFILE(,R2)
         ST    R14,DCBDCBE
*
         llgt  R14,WKPLISTA       LOAD  START-UP  DATA   ADDRESS
         llgt  R14,GPSTARTA-GENPARM(,R14)
         USING STRTDATA,R14
         if CLC,SDDDNPFX,ne,BLANKS    CHECK IF DDNAME PREFIX SPECIFIED
           MVC DCBDDNAM(L'SDDDNPFX),SDDDNPFX
         endif
         DROP  R14
*
*
         do ,                    enclosing do block (to allow leave)
           if CLI,GLOBPRNT,eq,C'Y'    FIRSTTIME ???
             MVI GLOBPRNT,C'N'
*
             MVC   REENTWK(8),OPENPARM
             OPEN  ((R2),OUTPUT),MODE=31,MF=(E,REENTWK) OPEN  FILE
             if tm,dcboflgs,dcbofopn,no  did it fail to open?
*
             GVBMSG FORMAT,MSGNO=OPEN_MERGRPT_FAIL,SUBNO=2,            +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(DCBDDNAM,l'DCBDDNAM),                             +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
               BRU RTNERROR
             endif
*
***********************************************************************
* CONTROL REPORT HEADER - CALL GVBUTHDR                               *
***********************************************************************
             lay   R1,HDRLIST
             USING HEADERPR,R1      LOAD PARMS TO DSECT TO CALL
             lay   r5,typepgm       gvbuthdr
             sty   r5,pgmtype
             lay   r5,typepgml      gvbuthdr
             sty   r5,pgmtypel
             lay   r5,namepgmx
             sty   r5,pgmname
             lay   r5,namepgmxl
             sty   r5,pgmnamel
             lay   r5,titpgm
             sty   r5,pgm_title
             lay   r5,titpgml
             sty   r5,pgm_title_ln
             lay   r5,rpt_95ctrl
             sty   r5,rpt_title
             lay   r5,rpt_95ctrll
             sty   r5,rpt_title_ln
             lay   r5,rpt_dd95xc
             sty   r5,rptddn
             lay   r5,reentwk
             sty   r5,rpt_reccnt
             lay   R3,buffer
             STy   R3,BUFFADD
             LAy   R5,BUFLGTH
             STy   R5,BUFFLGTH
             DROP R1
*
             llgf  R15,GVBHDRA      Call GVBUTHDR
             BASsm R14,R15
*
***********************************************************************
*  Print Control Report heading lines                                 *
***********************************************************************
*
             lh    r5,reentwk       Get number of lines in heading
*                                 includes blank lines
             mvc wkprttxt,headerb
             mvi wkprttxt,c'1'    move in carriage control
prntc_loop ds 0h
             lh    r4,0(r3)         get the length of the line
             bctr  r4,r0            decrement for "EX"
             exrl  r4,mvcprnt       move in header line
             lay   r0,wkprttxt      print it
             Put   (r2),(r0)        output header record
             mvc   wkprttxt,headerb
             la    r3,2+1(r4,r3)    next header line
             jct   r5,prntc_loop    Go print next line
*
             lay R0,headerb       BLANK LINE
             PUT (R2),(R0)
             lay R0,headerd       DASH  LINE;
             PUT (R2),(R0)
             lay R0,header05      "PARAMETER RECAP"
             PUT (R2),(R0)
             lay R0,headerb       BLANK LINE
             PUT (R2),(R0)
             lay R0,header06      COLUMN HEADINGS
             PUT (R2),(R0)
             lay R0,header07      COLUMN DASHES
             PUT (R2),(R0)
*
             lay R14,header08
             MVC WKPRTTXT,0(R14)
*
             llgt R7,WKPARMA
             USING PARMTBL,R7
             LA R4,MAXPARM
*
             do from=(r4),while=(clc,parment,ne,xhexff)
*
               MVI WKPRTTXT+0,C' '
               MVC WKPRTTXT+1(L'WKPRTTXT-1),WKPRTTXT+0
               select ,
                 when clc,BREAKLEN,eq,Parment
                   MVC STATENT(L'BREAKLEN),PARMENT
                   MVC STATRSRC,PARMRSRC

                 when clc,notfnd,eq,Parment
                   MVC STATENT(L'NOTFND),PARMENT
                   MVC STATRSRC,PARMRSRC

                 when clc,lrbuffer,eq,Parment
                   MVC STATENT(L'LRBUFFER),LRBUFFER
                   MVC STATRSRC,PARMRSRC

                 when clc,extenson,eq,Parment
                   MVC STATENT(L'EXTENSON),EXTENSON
                   MVC STATRSRC,PARMRSRC

                 when clc,ignorddn,eq,Parment
                   MVC STATENT(L'IGNORDDN),IGNORDDN

                 when clc,paddecmp,eq,Parment
                   MVC STATENT(L'PADDECMP),PADDECMP

                 when clc,bufrexit,eq,Parment
                   MVC STATENT(L'BUFREXIT),BUFREXIT
                   MVC STATRSRC,PARMRSRC

                 when clc,bufno,eq,Parment
                   MVC STATENT(L'BUFNO),BUFNO
                   MVC STATRSRC,PARMRSRC

                 when clc,ptrtblsz,eq,Parment
                   MVC STATENT(L'PTRTBLSZ),PTRTBLSZ               @020I
                   MVC STATRSRC,PARMRSRC                          @020I

                 When CLC,BUFSIZE,EQ,Parment
                   MVC STATENT(L'BUFSIZE),BUFSIZE
                   MVC STATRSRC,PARMRSRC

                 When CLC,TBUFSIZE,EQ,Parment
                   MVC STATENT(L'TBUFSIZE),TBUFSIZE
                   MVC STATRSRC,PARMRSRC

                 othrwise ,
                   MVC STATENT,PARMENT
                   MVC STATDDNP,PARMDDN
                   MVC STATRSRC,PARMRSRC
                   MVC STATTPRT+0(2),PARMPFR
                   MVC STATTPRT+2(1),=C'/'
                   MVC STATTPRT+3(2),PARMPTO
                   MVC STATETYP,PARMETYP
                   MVC STATCLAP,PARMCLAP
                   MVC STATDB2,PARMDB2
                   MVC STATGDG,PARMGDG
*
                   select ,
                     when CLC,PARMKXIT,ne,BLANKS
                       MVC STATDSN,BLANKS
                       MVC STATDSN(L'PARMKXIT),PARMKXIT
                       MVC STATDSN+L'PARMKXIT+1(L'PARMRXPM),PARMRXPM

                     when CLC,PARMRXIT,ne,BLANKS
                       MVC STATDSN,BLANKS
                       MVC STATDSN(L'PARMRXIT),PARMRXIT
                       MVC STATDSN+L'PARMRXIT+1(L'PARMRXPM),PARMRXPM

                     othrwise ,
                       MVC STATDSN,PARMDSN

                   endsel
                   MVC STATMAP,BLANKS
*
                   if OC,PARMOMAP,PARMOMAP,nz
                     MVC STATMAP,PARMOMAP
                   endif
*
                   if OC,PARMKMAP,PARMKMAP,nz
                     MVC STATMAP,PARMKMAP
                   endif
*
                   LH R0,PARMPFXL
                   CVD R0,WKDBLWRK
                   OI WKDBLWRK+L'WKDBLWRK-1,X'0F'
                   UNPK STATPFXL,WKDBLWRK
*
                   LH R0,PARMKEYO
                   AHI R0,1
                   CVD R0,WKDBLWRK
                   OI WKDBLWRK+L'WKDBLWRK-1,X'0F'
                   UNPK STATKEYO,WKDBLWRK
                   if CP,WKDBLWRK,le,P99
                     MVI STATKEYO,C' '
                   endif
*
                   LH R0,PARMKEYL
                   CVD R0,WKDBLWRK
                   OI WKDBLWRK+L'WKDBLWRK-1,X'0F'
                   UNPK STATKEYL,WKDBLWRK
                   if CP,WKDBLWRK,le,P99
                     MVI STATKEYL,C' '
                   endif
*
                   LH R0,PARMBRKL
                   CVD R0,WKDBLWRK
                   OI WKDBLWRK+L'WKDBLWRK-1,X'0F'
                   UNPK STATBRKL,WKDBLWRK
                   if Cli,statbrkl,eq,c'0'
                     MVI STATbrkl,C' '
                   endif
*
                   LH R0,PARMTMSO
                   AHI R0,1
                   CVD R0,WKDBLWRK
                   OI WKDBLWRK+L'WKDBLWRK-1,X'0F'
                   UNPK STATTMSO,WKDBLWRK
                   if Cli,stattmso,eq,c'0'
                     MVI STATtmso,C' '
                   endif
*
                   LH R0,PARMEFDO
                   AHI R0,1
                   CVD R0,WKDBLWRK
                   OI WKDBLWRK+L'WKDBLWRK-1,X'0F'
                   UNPK STATEFDO,WKDBLWRK
                   if Cli,statefdo,eq,c'0'
                     MVI statefdo,C' '
                   endif
*
                   LH R0,PARMPROO
                   AHI R0,1
                   CVD R0,WKDBLWRK
                   OI WKDBLWRK+L'WKDBLWRK-1,X'0F'
                   UNPK STATPROO,WKDBLWRK
                   if Cli,statproo,eq,c'0'
                     MVI statproo,C' '
                   endif
*
                 endsel

               PUT (R2),WKPRTTXT
*
               ahi R7,PARMLEN
             enddo
*
             DROP R7
*
             MVC   REENTWK(8),OPENPARM
             CLOSE ((R2)),MODE=31,MF=(E,REENTWK)   CLOSE FILE
*
           endif
         enddo
*
*        DEQ   (GENEVA,LPGMNAME,,STEP),RNL=NO
         MVC   REENTWK(MDLDEQPL),MDLDEQP
         DEQ   MF=(E,REENTWK)
*
         lmg   R14,R12,WKSAVSUB         RESTORE REGISTERS
         bsm   0,r14                    RETURN
*
         ds    0d
mvcprnt      mvc  wkprttxt+1(0),2(r3)
*
         DROP  R2
         DROP  R3
         DROP  R5
         DROP  R6
*
         DROP  R13
                        EJECT
***********************************************************************
*   FILL-IN PARTITION TOKEN AREA FIELDS                               *
*   RETURN  PARTITION TOKEN AREA ADDRESS                              *
*                                                                     *
*   CALL GENCKTKN USING THREAD-DDNAME               PIC  X(8)      +0 *
*                       ENTITY-ID                   PIC  X(8)      +4 *
*                       PARTITION-DATA-AREA-POINTER POINTER        +8 *
*                       1ST-FILE-INPUT-REC-CNT-PTR  POINTER        +12*
*                       1ST-FILE-RECORD-LEN-PTR     POINTER        +16*
*                       RETURN-CODE                 PIC S9(8) COMP +20*
*                                                                     *
*   RETURN CODES:                                                     *
*        0   - SUCCESSFUL                                             *
*        4   - ENTITY ID NOT FOUND                                    *
*        8   - END-OF-BROWSE                                          *
*        16  - THREAD DDNAME TOKEN NOT FOUND                          *
*                                                                     *
***********************************************************************
               sysstate amode64=NO
GENCKTKN ds    0H
*
         using saver,r13          map the save area
         STM   R14,R12,savgrs14   SAVE  CALLER'S  REGISTERS
*
         LArl  R12,gvbxrck        RE-ESTABLISH  PROGRAM BASE REGISTERS
         USING (GVBXRCK,code),R12
*
         LR    R9,R1              SAVE PARAMETER LIST ADDRESS
         srlg  r0,r0,32           move high half of r0 to low half
         srlg  r1,r1,32           move high half of r1 to low half
         srlg  r15,r15,32         move high half of r15 to low half
         lr    r2,r0              save high r0 in low r2
         lr    r3,r1              save high r1 in low r3
         lr    r4,r15             save high r15 in low r4

*
         LHI   R0,epworklN+8       LOAD   WORK AREA SIZE
         STORAGE OBTAIN,LENGTH=(0),LOC=(ANY),CHECKZERO=YES GET WORKAREA
         if    cij,r15,ne,x'14'   not zeroed?
           LR  r10,r1             save address
           LR  R0,R1              ZERO  WORK  AREA
           LHI R1,epworkln+8
           SR  R14,R14
           SR  R15,R15
           MVCL R0,R14
           lr  r1,r10             restore pointer
         endif
*
         MVC   0(8,R1),CKTKEYEB
         aghi  r1,l'cktkeyeb      move pointer past
         drop  r13
         USING epworkar,R1
         using savf5sa,epworkar
         stg   r13,savf5saprev    save current r13
         stmh  r2,r14,savf5sag64hs2    save high parts r2 - r14
         st    r2,savf5sag64hs0  save high parts of r0
         st    r3,savf5sag64hs1                      r1
         st    r4,savf5sag64hs15                and r15
         mvc   savf5said(4),=a(savf5said_value) set 'F5SA' in area
         lgr   R13,r1             Get new workarea into r13
         drop  r1
         USING epworkar,R13
         using savf5sa,epworkar
*
*
         MVC   EPTOKNAM+0(8),GENEVA
         L     R14,0(,R9)
         MVC   EPTOKNAM+8(8),0(R14)
*
         CALL  IEANTRT,(TOKNLVL1,EPTOKNAM,EPTOKN,EPTOKNRC),            +
               MF=(E,EPREENT)
*
         L     R15,EPTOKNRC       SUCCESSFUL  ???
         LTR   R15,R15
         BRZ   TKNFOUND           YES - CONTINUE
*
         LHI   R15,16             RETURN "NOT FOUND"
         BRU   TKNSETRC
*
TKNFOUND L     R6,EPTOKNPA        LOAD PARTITION DATA AREA   ADDRESS
         USING PARTAREA,R6
*
         L     R7,PARTGLOB        LOAD GLOBAL AREA ADDRESS
         USING GLOBAREA,R7
*
         L     R8,PARTWORK        LOAD THREAD WORK    AREA   ADDRESS
         USING XRCKWORK,R8
*
***********************************************************************
*  SEARCH FILE CONTROL AREAS FOR MATCHING "ENTITY ID"                 *
***********************************************************************
         L     R5,WKFILBEG        LOAD FIRST  FILE CONTROL BLOCK ADR
         USING FILEAREA,R5
*
         L     R14,WKOPNBEG       ANY  EVENT  FILES   OPEN ???
         CLC   0(4,R14),XHEXFF
         BRE   TKNSET4            NO - RETURN (RC=4)
*
         L     R1,4(,R9)          LOAD ENTITY ID   ADDRESS
*
TKNLOOP  CLC   FILERTYP,0(R1)
         BRE   TKNPART
*
         AHI   R5,FILELEN
         C     R5,WKFILMAX
         BRL   TKNLOOP
*
TKNSET4  LHI   R15,4
         BRU   TKNSETRC
*
TKNPART  L     R14,08(,R9)        LOAD   PARTITION AREA POINTER  ADDR
         ST    R6,0(,R14)         RETURN PARTITION AREA ADDRESS
*
         L     R14,12(,R9)        LOAD RECORD  COUNT  POINTER ADDRESS
         LA    R0,FILEICNT        SWITCH END-OF-DATA  POINTER POINTER
         ST    R0,0(,R14)
*
         L     R14,16(,R9)        LOAD RECORD LENGTH  POINTER ADDRESS
         LA    R0,FILERECL        SWITCH END-OF-DATA  POINTER POINTER
         ST    R0,0(,R14)
*
         SR    R15,R15
*
tknsetrc lgr   r1,r13                  copy current area
         aghi  r1,-8                     and move to start
         lmh   r2,r14,savf5sag64hs2    Restore high halves
         lg    r13,savf5saprev         restore caller save area
         L     R14,20(,R9)
         ST    R15,0(,R14)
         STORAGE RELEASE,LENGTH=EPWORKLN+8,ADDR=(1)
         using saver,r13
         l     r14,savgrs14            restore caller's r14
         lm    r2,r12,savgrs2          restore caller's r2- r12
         BSM   0,R14              RETURN
*
*
         DROP  R5
         DROP  R6
         DROP  R7
         DROP  R8
         drop  r13
                        EJECT
***********************************************************************
*              G E T   D I R E C T O R Y   A D D R E S S              *
*                                                                     *
*   ALLOCATE WORK AREA                                                *
*   LOCATE   DIRECTORY FOR REQUESTED  ENTITY                          *
*   RETURN   DIRECTORY ADDRESS   IN   POINTER                         *
*   RELEASE  WORK AREA                                                *
*                                                                     *
*   CALL GENCKDIR USING THREAD-DDNAME             PIC  X(8)       +0  *
*                       ENTITY-ID                 PIC  X(8)       +4  *
*                       RETURN-DIR-POINTER        POINTER         +8  *
*                       RETURN-CODE               PIC S9(8) COMP  +12 *
*                                                                     *
*   RETURN CODES:                                                     *
*        0   - SUCCESSFUL                                             *
*        4   - ENTITY ID NOT FOUND                                    *
*                                                                     *
***********************************************************************
GENCKdir ds    0H
*
         using saver,r13          map the save area
         STM   R14,R12,savgrs14   SAVE  CALLER'S  REGISTERS
*
         LArl  R12,gvbxrck        RE-ESTABLISH  PROGRAM BASE REGISTERS
         USING (GVBXRCK,code),R12
*
         LR    R9,R1              SAVE PARAMETER LIST ADDRESS
*
         srlg  r0,r0,32           move high half of r0 to low half
         srlg  r1,r1,32           move high half of r1 to low half
         srlg  r15,r15,32         move high half of r15 to low half
         lr    r2,r0              save high r0 in low r2
         lr    r3,r1              save high r1 in low r3
         lr    r4,r15             save high r15 in low r4

*
         LHI   R0,epworklN         LOAD   WORK AREA SIZE
         STORAGE OBTAIN,LENGTH=(0),LOC=(ANY),CHECKZERO=YES GET WORKAREA
         if    cij,r15,ne,x'14'   not zeroed?
           LR  r10,r1             save address
           LR  R0,R1              ZERO  WORK  AREA
           LHI R1,epworkln
           SR  R14,R14
           SR  R15,R15
           MVCL R0,R14
           lr  r1,r10             restore pointer
         endif
*
         drop  r13
         USING epworkar,R1
         using savf5sa,epworkar
         stg   r13,savf5saprev    save current r13
         stmh  r2,r14,savf5sag64hs2    save high parts r2 - r14
         st    r2,savf5sag64hs0  save high parts of r0
         st    r3,savf5sag64hs1                      r1
         st    r4,savf5sag64hs15                and r15
         mvc   savf5said(4),=a(savf5said_value) set 'F5SA' in area
         lgr   R13,r1             Get new workarea into r13
         drop  r1
         USING epworkar,R13
         using savf5sa,epworkar
*
         MVC   EPTOKNAM+0(8),GENEVA
         L     R14,0(,R9)
         MVC   EPTOKNAM+8(8),0(R14)
*
* Not ideal, but R14 is pointing to event ddname: GPDDNAME, which is
* located within the thread in area mapped by GENFILE. GPDDNAME is
* first.
* So by substracting offset of file_area in the thread we obtain the
* address of the thread itself.
*
         AHI   R14,-file_area+THRDAREA
         USING THRDAREA,R14
*
         L     R1,IMSWADDR
         MVC   EPTOKEN,0(R1)
         DROP  R14
*
         L     R14,EPTOKNPA       LOAD "GVBXRCK" WORK AREA ADDRESS
         L     R14,PARTWORK-PARTAREA(,R14)
         ST    R14,EPXRCKWK
*
         L     R14,4(,R9)         LOAD  ENTITY ID  ADDRESS
         MVC   EPENTID,0(R14)     SAVE  ENTITY ID
*
         L     R6,EPTOKNFA        LOAD  FILE AREA  ADDRESS
         USING FILEAREA,R6
*
GVBDIR20 CLC   FILERTYP,XHEXFF    END-OF-TABLE   ???
         BRE   GVBDIR40           YES - INDICATE ERROR
*
         CLC   FILERTYP,EPENTID   MATCHING ENTITY TYPE ???
         BRE   GVBDIR30
*
         AHI   R6,FILELEN
         BRU   GVBDIR20
*
GVBDIR30 LT    R5,FILEMINA        SAVE DIRECTORY ADDRESS
         BRZ   GVBDIR40
*
         ST    R5,EPDIRAD
*
         DROP  R6
*
         L     R14,8(,R9)         LOAD POINTER   ADDRESS
         ST    R5,0(,R14)
*
         SR    R15,R15            INDICATE  SUCCESSFUL
         BRU   GVBDIR99
*
GVBDIR40 LHI   R15,4              INDICATE ENTITY NOT FOUND
*
GVBDIR99 ST    R15,EPLASTRC       SAVE LAST RETURN CODE
*
         lgr   r1,r13                  copy current area
         lmh   r2,r14,savf5sag64hs2    Restore high halves
         lg    r13,savf5saprev         restore caller save area
         L     R14,12(,R9)
         ST    R15,0(,R14)
         STORAGE RELEASE,LENGTH=EPWORKLN,ADDR=(1)
         using saver,r13
         l     r14,savgrs14            restore caller's r14
         lm    r2,r12,savgrs2          restore caller's r2- r12
         BSM   0,R14              RETURN
*
         DROP  R13
                        EJECT
***********************************************************************
*              S T A R T   E N T I T Y   B R O W S E                  *
*                                                                     *
*   ALLOCATE WORK AREA IF  FIRST TIME CALLED                          *
*   LOCATE   DIRECTORY FOR REQUESTED  ENTITY                          *
*   RETURN   FIRST  RECORD POINTER                                    *
*                                                                     *
*   CALL GENCKSB USING THREAD-DDNAME              PIC  X(8)       +0  *
*                      ENTITY-ID                  PIC  X(8)       +4  *
*                      RETURN-REC-POINTER         POINTER         +8  *
*                      RETURN-REC-LEN             PIC S9(8) COMP  +12 *
*                      GENCKSB-WORK-AREA-ANCHOR   POINTER         +16 *
*                      RETURN-CODE                PIC S9(8) COMP  +20 *
*                                                                     *
*   RETURN CODES:                                                     *
*        0   - SUCCESSFUL                                             *
*        4   - ENTITY ID NOT FOUND                                    *
*        8   - END-OF-BROWSE                                          *
*        16  - THREAD DDNAME TOKEN NOT FOUND                          *
*                                                                     *
***********************************************************************
GENCKsb  ds    0H
*
         using saver,r13          map the save area
         STM   R14,R12,savgrs14   SAVE  CALLER'S  REGISTERS
*
         LArl  R12,gvbxrck        RE-ESTABLISH  PROGRAM BASE REGISTERS
         USING (GVBXRCK,code),R12
*
         LR    R9,R1              SAVE PARAMETER LIST ADDRESS
*
         srlg  r0,r0,32           move high half of r0 to low half
         srlg  r1,r1,32           move high half of r1 to low half
         srlg  r15,r15,32         move high half of r15 to low half
         lr    r2,r0              save high r0 in low r2
         lr    r3,r1              save high r1 in low r3
         lr    r4,r15             save high r15 in low r4

         L     R8,16(,R9)         LOAD WORK AREA ANCHOR ADDRESS
         Lt    R1,0(,R8)          WORK AREA ALREADY   ALLOCATED ???
*
         BRP   GVBSB_10
*
         LHI   R0,epworklN         LOAD   WORK AREA SIZE
         STORAGE OBTAIN,LENGTH=(0),LOC=(ANY),CHECKZERO=YES GET WORKAREA
         if    cij,r15,ne,x'14'   not zeroed?
           LR  r10,r1             save address
           LR  R0,R1              ZERO  WORK  AREA
           LHI R1,epworkln
           SR  R14,R14
           SR  R15,R15
           MVCL R0,R14
           lr  r1,r10             restore pointer
         endif
*
         drop  r13
         USING epworkar,R1
         using savf5sa,epworkar
         stg   r13,savf5saprev    save current r13
         stmh  r2,r14,savf5sag64hs2    save high parts r2 - r14
         st    r2,savf5sag64hs0   save high parts of r0
         st    r3,savf5sag64hs1                      r1
         st    r4,savf5sag64hs15                and r15
         mvc   savf5said(4),=a(savf5said_value) set 'F5SA' in area
         ST    R1,0(,R8)          save the new wkarea address
         lgr   R13,r1             Get new workarea into r13
         push  using
         drop  r1
         USING epworkar,R13
         using savf5sa,epworkar
*
         MVC   EPTOKNAM+0(8),GENEVA
         L     R14,0(,R9)
         MVC   EPTOKNAM+8(8),0(R14)
*                                                                 @007I
* Not ideal, but R14 is pointing to event ddname: GPDDNAME, which is
* located within the thread in area mapped by GENFILE. GPDDNAME is
* first.
* So by substracting offset of file_area in the thread we obtain the
* address of the thread itself.
*
         AHI   R14,-file_area+THRDAREA
         USING THRDAREA,R14                                       @007I
         L     R1,IMSWADDR                                        @007I
         MVC   EPTOKEN,0(R1)                                      @007I
         DROP  R14                                                @007I
*
*        CALL  IEANTRT,(TOKNLVL1,EPTOKNAM,EPTOKN,EPTOKNRC),       @007C
*              MF=(E,EPREENT)                                     @007C
*        LA    R14,TOKNLVL1                                       @007C
*        LA    R15,EPTOKNAM                                       @007C
*        LA    R0,EPTOKN                                          @007C
*        LA    R1,EPTOKNRC                                        @007C
*        STM   R14,R1,EPREENT                                     @007C
*        LA    R1,EPREENT                                         @007C
*        L     R15,IEANTRT                                        @007C
*        BASR  R14,R15                                            @007C
*
*        L     R15,EPTOKNRC       SUCCESSFUL  ???                 @007C
*        LTR   R15,R15                                            @007C
*        BRZ   GVBSB_05           YES - CONTINUE                  @007C
*
*        LHI   R15,16             RETURN "NOT FOUND"              @007C
*        BRU   GVBSB_99                                           @007C
*
GVBSB_05 L     R14,EPTOKNPA       LOAD "GVBXRCK" WORK AREA ADDRESS
         L     R14,PARTWORK-PARTAREA(,R14)
         ST    R14,EPXRCKWK
         BRU   GVBSB_15
*
         pop   using
gvbsb_10 ds    0h
         stg   r13,savf5saprev    save current r13
         stmh  r2,r14,savf5sag64hs2    save high parts r2 - r14
         st    r2,savf5sag64hs0  save high parts of r0
         st    r3,savf5sag64hs1                      r1
         st    r4,savf5sag64hs15                and r15
         llgtr R13,r1             Get new workarea into r13
         drop  r1
         USING epworkar,R13
         using savf5sa,epworkar
*
GVBSB_15 L     R14,4(,R9)         LOAD  ENTITY ID  ADDRESS
         CLC   EPENTID,0(R14)     SAME  ENTITY ???
         BRE   GVBSB_30           YES - BYPASS SEARCH
*
         MVC   EPENTID,0(R14)     SAVE  ENTITY ID
*
         L     R6,EPTOKNFA        LOAD  FILE AREA  ADDRESS
         USING FILEAREA,R6
*
GVBSB_20 CLC   FILERTYP,XHEXFF    END-OF-TABLE   ???
         BRE   GVBSB_22           YES - INDICATE ERROR
*
         CLC   FILERTYP,EPENTID   MATCHING ENTITY TYPE ???
         BRE   GVBSB_25
*
         AHI   R6,FILELEN
         BRU   GVBSB_20
*
GVBSB_22 LHI   R15,4              INDICATE ENTITY NOT FOUND
         BRU   GVBSB_99
*
GVBSB_25 L     R5,FILEMINA        SAVE DIRECTORY ADDRESS
         ST    R5,EPDIRAD
*
         DROP  R6
*
GVBSB_30 L     R5,EPDIRAD         LOAD DIRECTORY ADDRESS
*
         CLC   0(4,R5),4(R5)      MIN = MAX  (NO RECORD POINTERS) ???
         BRNE  GVBSB_35
*
         LHI   R15,8              INDICATE END-OF-SCAN
         BRU   GVBSB_99
*
GVBSB_35 LGF   R14,0(,R5)         LOAD 1ST POINTER ADDRESS
         ST    R14,EPPTRAD        SAVE ADDRESS FOR "BROWSE" FUNCTION
         LG    R14,0(,R14)        LOAD 1ST RECORD  ADDRESS
*
         SAM64
         LGH   R15,0(,R14)        LOAD RECORD LENGTH ("RDW")
         SAM31
         AHI   R15,-4
         AGHI  R14,4
*
         LGF   R1,8(,R9)          RETURN RECORD ADDRESS
         STG   R14,0(,R1)
         L     R1,12(,R9)         RETURN RECORD LENGTH
         ST    R15,0(,R1)
*
         SR    R15,R15            INDICATE  SUCCESSFUL
*
GVBSB_99 ST    R15,EPLASTRC       SAVE LAST RETURN CODE
*
         lgr   r1,r13                  copy current area
         lmh   r2,r14,savf5sag64hs2    Restore high halves
         lg    r13,savf5saprev         restore caller save area
         L     R14,20(,R9)
         ST    R15,0(,R14)
         using saver,r13
         l     r14,savgrs14            restore caller's r14
         lm    r2,r12,savgrs2          restore caller's r2- r12
         BSM   0,R14              RETURN
*
         DROP  R13
                        EJECT
***********************************************************************
*              B R O W S E   E N T I T Y                              *
*                                                                     *
*   CONTINUE BROWSING SAME ENTITY AS "START BROWSE"                   *
*                                                                     *
*   CALL GENCKBR USING THREAD-DDNAME              PIC  X(8)       +0  *
*                      ENTITY-ID                  PIC  X(8)       +4  *
*                      RETURN-REC-POINTER         POINTER         +8  *
*                      RETURN-REC-LEN             PIC S9(8) COMP  +12 *
*                      GENCKBR-WORK-AREA-ANCHOR   POINTER         +16 *
*                      RETURN-CODE                PIC S9(8) COMP  +20 *
*                                                                     *
*   RETURN CODES:                                                     *
*        0   - SUCCESSFUL                                             *
*        4   - ENTITY ID NOT FOUND                                    *
*        8   - END-OF-BROWSE                                          *
*        12  - WORK AREA ANCHOR MISSING                               *
*                                                                     *
***********************************************************************
GENCKbr  ds    0H
*
         using saver,r13          map the save area
         STM   R14,R12,savgrs14   SAVE  CALLER'S  REGISTERS
*
         LArl  R12,gvbxrck        RE-ESTABLISH  PROGRAM BASE REGISTERS
         USING (GVBXRCK,code),R12
*
         LR    R9,R1              SAVE PARAMETER LIST ADDRESS
         srlg  r0,r0,32           move high half of r0 to low half
         srlg  r1,r1,32           move high half of r1 to low half
         srlg  r15,r15,32         move high half of r15 to low half
         lr    r2,r0              save high r0 in low r2
         lr    r3,r1              save high r1 in low r3
         lr    r4,r15             save high r15 in low r4

         L     R8,16(,R9)         LOAD WORK AREA ANCHOR ADDRESS
         L     R1,0(,R8)
*
         LTR   R1,R1              WORK AREA ALREADY   ALLOCATED ???
*
         BRP   GVBBR_10
*
         LHI   R15,12             RETURN WORK AREA MISSING
         BRU   GVBBR_99             and only restore low halves
*
GVBBR_10 ds    0h
         USING epworkar,R1
         using savf5sa,epworkar
         stg   r13,savf5saprev    save current r13
         stmh  r2,r14,savf5sag64hs2    save high parts r2 - r14
         st    r2,savf5sag64hs0   save high parts of r0
         st    r3,savf5sag64hs1                      r1
         st    r4,savf5sag64hs15                and r15
         llgtr R13,r1             Get new workarea into r13
         drop  r1
         USING epworkar,R13
         using savf5sa,epworkar
         L     R14,4(,R9)         LOAD  ENTITY ID  ADDRESS
         CLC   EPENTID,0(R14)     SAME  ENTITY ???
         BRE   GVBBR_20           YES - BYPASS SEARCH
*
         LHI   R15,4              INDICATE ENTITY NOT FOUND
         BRU   GVBBR_98
*
GVBBR_20 L     R5,EPDIRAD         LOAD DIRECTORY    ADDRESS
         LGF   R14,EPPTRAD        LOAD LAST POINTER ADDRESS
         Aghi  R14,8
         ST    R14,EPPTRAD        SAVE NEXT POINTER ADDRESS
*
         C     R14,4(,R5)         MAX  REACHED  ???
         BRL   GVBBR_30           NO  - RETURN NEXT RECORD
*
         LHI   R15,8              INDICATE END-OF-SCAN
         BRU   GVBBR_98
*
GVBBR_30 LG    R14,0(,R14)        LOAD NEXT RECORD  ADDRESS
*
         SAM64
         LGH   R15,0(,R14)        LOAD RECORD LENGTH ("RDW")
         SAM31
         AHI   R15,-4
         AGHI  R14,4
*
         L     R1,8(,R9)          RETURN RECORD ADDRESS
         STG   R14,0(,R1)
         L     R1,12(,R9)         RETURN RECORD LENGTH
         ST    R15,0(,R1)
*
         SR    R15,R15            INDICATE  SUCCESSFUL
*
GVBBR_98 ST    R15,EPLASTRC       SAVE LAST RETURN CODE
*
         lgr   r1,r13                  copy current area
         lmh   r2,r14,savf5sag64hs2    Restore high halves
         lg    r13,savf5saprev         restore caller save area
GVBBR_99 ds    0h                 PASS RETURN CODE
         L     R14,20(,R9)
         ST    R15,0(,R14)
         using saver,r13
         l     r14,savgrs14            restore caller's r14
         lm    r2,r12,savgrs2          restore caller's r2- r12
         BSM   0,R14              RETURN
*
         DROP  R13
                        EJECT
***********************************************************************
*      A D D   G E N E R A T E D   R E C O R D   T O   B U F F E R    *
*                                                                     *
*   IF PRIMARY BUFFER IS FULL, CLOSE OUT LAST BLOCK IN LR BUFFER      *
*   BUILD BDW FOR NEXT BLOCK IN  LR BUFFER EXTENSION AREA             *
*   RECORD POINTERS FOR ADDITIONAL RECORDS WILL POINT TO EXTENSION    *
*                                                                     *
*   CALL GENCKTRN USING THREAD-DDNAME              PIC  X(8)       +0 *
*                       OUTPUT-REC ADDRESS         PIC  X(?)       +4 *
*                       GENCKTRN-WORK-AREA-ANCHOR  POINTER         +8 *
*                       RETURN-CODE                PIC S9(8) COMP  +12*
*                                                                     *
*   RETURN CODES:                                                     *
*        0   - SUCCESSFUL                                             *
*        4   - ENTITY ID NOT FOUND                                    *
*        8   - END-OF-BROWSE                                          *
*        16  - THREAD DDNAME TOKEN NOT FOUND                          *
*                                                                     *
***********************************************************************
GENCKTRN ds    0H
*
         using saver,r13          map the save area
         STM   R14,R12,savgrs14   SAVE  CALLER'S  REGISTERS
*
         LArl  R12,gvbxrck       RE-ESTABLISH  PROGRAM BASE REGISTERS
         USING (GVBXRCK,code),R12
         LR    R9,R1              SAVE PARAMETER LIST ADDRESS
*
         srlg  r0,r0,32           move high half of r0 to low half
         srlg  r1,r1,32           move high half of r1 to low half
         srlg  r15,r15,32         move high half of r15 to low half
         lr    r2,r0              save high r0 in low r2
         lr    r3,r1              save high r1 in low r3
         lr    r4,r15             save high r15 in low r4

         L     R8,8(,R9)         LOAD WORK AREA ANCHOR ADDRESS
         LT    R1,0(,R8)         WORK AREA ALREADY   ALLOCATED ???
         BRP   GVBGN_15
*
*
         LHI   R0,epworklN+8       LOAD   WORK AREA SIZE
         STORAGE OBTAIN,LENGTH=(0),LOC=(ANY),CHECKZERO=YES GET WORKAREA
         if    cij,r15,ne,x'14'   not zeroed?
           LR  r10,r1             save address
           LR  R0,R1              ZERO  WORK  AREA
           LHI R1,epworkln+8
           SR  R14,R14
           SR  R15,R15
           MVCL R0,R14
           lr  r1,r10             restore pointer
         endif
*
         MVC   0(8,R1),CKTREYEB
         aghi  r1,l'cktkeyeb      move pointer past
         drop  r13
         USING epworkar,R1
         using savf5sa,epworkar
         stg   r13,savf5saprev    save current r13
         stmh  r2,r14,savf5sag64hs2    save high parts r2 - r14
         st    r2,savf5sag64hs0  save high parts of r0
         st    r3,savf5sag64hs1                      r1
         st    r4,savf5sag64hs15                and r15
         mvc   savf5said(4),=a(savf5said_value) set 'F5SA' in area
         lgr   R13,r1             Get new workarea into r13
         ST    R13,0(,R8)
         push  using
         drop  r1
         USING epworkar,R13
         using savf5sa,epworkar
*
*
         MVC   EPTOKNAM+0(8),GENEVA
         L     R14,0(,R9)
         MVC   EPTOKNAM+8(8),0(R14)
*
* Not ideal, but R14 is pointing to event ddname: GPDDNAME, which is
* located within the thread in area mapped by GENFILE. GPDDNAME is
* first.
* So by substracting offset of file_area in the thread we obtain the
* address of the thread itself.
*
         AHI   R14,-file_area+THRDAREA
         USING THRDAREA,R14
         L     R1,IMSWADDR
         MVC   EPTOKEN,0(R1)
         DROP  R14
*
*        CALL  IEANTRT,(TOKNLVL1,EPTOKNAM,EPTOKN,EPTOKNRC),
*              MF=(E,EPREENT)
*        LA    R14,TOKNLVL1
*        LA    R15,EPTOKNAM
*        LA    R0,EPTOKN
*        LA    R1,EPTOKNRC
*        STM   R14,R1,EPREENT
*        LA    R1,EPREENT
*        L     R15,IEANTRT
*        BASR  R14,R15
*
*        L     R15,EPTOKNRC       SUCCESSFUL ???
*        LTR   R15,R15
*        BRZ   GVBGN_10           YES - CONTINUE
*
*        LHI   R15,16             RETURN "TOKEN MISSING"
*        BRU   GVBGN_99
*
GVBGN_10 L     R8,EPTOKNPA        LOAD "GVBXRCK" WORK AREA ADDRESS
         L     R8,PARTWORK-PARTAREA(,R8)
         USING XRCKWORK,R8
*
         ST    R8,EPXRCKWK        SAVE  ADDRESS
         BRU   GVBGN_20
*
         pop   using
gvbgn_15 ds    0h
         stg   r13,savf5saprev    save current r13
         stmh  r2,r14,savf5sag64hs2    save high parts r2 - r14
         st    r2,savf5sag64hs0  save high parts of r0
         st    r3,savf5sag64hs1                      r1
         st    r4,savf5sag64hs15                and r15
         llgtr R13,r1             Get new workarea into r13
         drop  r1
         USING epworkar,R13
         using savf5sa,epworkar
*
         L     R8,EPXRCKWK        LOAD  BASE REGISTER
         USING XRCKWORK,R8
*
GVBGN_20 ds    0h                 SWITCH  TO 64-BIT MODE
         llgtr r12,r12            clean r12
         LLGTR R8,R8              CLEAR HIGH HALF
         LLGTR R9,R9
         SAM64                    SWITCH  TO 64-BIT MODE
*
*
         LG    R7,WKGENEND        COMPUTE LENGTH OF DATA IN BUFFER
         LGR   R14,R7
         LG    R5,WKGENBDW
         SGR   R14,R5
*
         Llgt  R2,4(,R9)          LOAD  GENERATED TRANSACTION REC ADDR
         LGH   R15,0(,R2)         LOAD  GENERATED RECORD   LENGTH
*
         LGR   R0,R7              BUFFER OVERFLOW ???
         AGR   R0,R15
         AGHI  R0,8
         CG    R0,WKGENMAX
         BRH   GVBGN_25           YES there is a buffer overflow
*
         LA    R0,0(R14,R15)      BLOCK  OVERFLOW ???
         CGfi  R0,BLKSIZE
         BRNH  GVBGN_80           NO  - CONTINUE
*
*
***********************************************************************
*  CLOSE OUT PREVIOUS BLOCK (BDW) AND START NEW BLOCK (BDW)           *
***********************************************************************
         STH   R14,0(,R5)         SET   BDW  LENGTH
*
         STG   R7,WKGENBDW        START NEW  BDW
         XGR   R15,R15
         STG   R15,0(,R7)
         AGHI  R7,4
         STG   R7,WKGENEND
*
         if CG,R7,gt,WKBLKHI         NEW "HIGHWATER MARK" ???
           STG R7,WKBLKHI
         endif
*
         BRU   GVBGN_20           COPY  RECORD  INTO  NEW  BLOCK
*
*
***********************************************************************
*   IF THERE IS AN "OVERFLOW" OF "LR BUFFER EXTENSION" DISPLAY ERROR  *
*   OTHERWISE ASSIGN EXTENSION TO THIS THREAD TO HANDLE OVERFLOW      *
***********************************************************************
GVBGN_25 ds    0h
*
         LGF   R15,4(,R9)         LOAD  GENERATED TRANSACTION REC ADDR
*        AHI   R15,4              SKIP  RDW
*
         GVBMSG FORMAT,MSGNO=CK_BUFFER_EXT_OVERFLOW,SUBNO=3,           +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(GENCKEPG,L'GENCKEPG),                             +
               SUB3=(4(r15),24),                 record/key            +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
*
         Llgt  R2,WKPLISTA
         USING GENPARM,R2
*
         Llgt  R2,GPENVA          LOAD   ENVIRONMENT INFO   ADDRESS
         USING GENENV,R2
*
         LH    R15,WKTXTBUF       RETURN TEXT LENGTH
         ST    R15,GP_ERROR_BUFFER_LEN
*
         BCTR  R15,0              DECREMENT   LENGTH FOR  "EX"
         Llgt  R14,GP_ERROR_BUFFER_PTR ERROR MESSAGE BUFFER ADDRESS
         LA    R1,WKTXTBUF+4
         exrl  R15,MVCR14R1
*
         LGHI  R15,16             SET  RETURN CODE REGISTER TO 16
         ST    R15,GP_ERROR_REASON
*
         LGHI  R15,16
         BRU   GVBGN_99
*
         DROP  R2
*
GVBGN_80 STH   R0,0(,R5)          UPDATE "BDW"
*
         CVD   R0,WKDBLWRK
         AP    WKGENBYT,WKDBLWRK
         agsi  WKGENCNT,b001
*
         if LTG,R14,WKRECPRV,p    CLEAR PREVIOUS LAST RECORD FLAG
           NI  2(R14),255-X'40'
         endif
         STG   R7,WKRECPRV        UPDATE  PREVIOUS RECORD ADDR
*
         LGR   R1,R7
         LGR   R14,R2
         LA    R15,255(,R15)
         SRLG  R0,R15,8
         BRU   GVBGN_85
GVBGN_82 MVC   0(256,R1),0(R14)   COPY  256 BYTES
         LA    R1,256(,R1)        ADVANCE  TARGET
         LA    R14,256(,R14)      ADVANCE  SOURCE
GVBGN_85 BRCT  R0,GVBGN_82
         exrl  R15,MVCR1R14       COPY  REMAINDER
*
         OI    2(R7),X'40'        UPDATE  LAST RECORD  FLAG
*
         nilf  R15,X'000000FF'    CLEAR HIGH ODER LENGTH BYTES
         LA    R0,1(R1,R15)       UPDATE  CURRENT ENDING ADDRESS
         STG   R0,WKGENEND
*
         if CG,R0,gt,WKBLKHI         NEW "HIGHWATER MARK" ???
           STG R0,WKBLKHI
         endif
*
         SR    R15,R15
*
GVBGN_99 ds    0h
*
         ST    R15,EPLASTRC       SAVE LAST RETURN CODE
*
         lmh   r2,r14,savf5sag64hs2    Restore high halves
         lg    r13,savf5saprev         restore caller save area
         L     R14,12(,R9)
         ST    R15,0(,R14)
         using saver,r13
         l     r14,savgrs14            restore caller's r14
         lm    r2,r12,savgrs2          restore caller's r2- r12
         BSM   0,R14              RETURN
*
*
         DROP  R8
         DROP  R13
                        EJECT
***********************************************************************
*      A D D   G E N E R A T E D   R E C O R D   T O   B U F F E R    *
*                                                                     *
*   IF PRIMARY BUFFER IS FULL, CLOSE OUT LAST BLOCK IN LR BUFFER      *
*   BUILD BDW FOR NEXT BLOCK IN  LR BUFFER EXTENSION AREA             *
*   RECORD POINTERS FOR ADDITIONAL RECORDS WILL POINT TO EXTENSION    *
*                                                                     *
*   CALL GENCKVRT USING THREAD-DDNAME              PIC  X(8)       +0 *
*                       OUTPUT-REC-POINTER         POINTER         +4 *
*                       GENCKVRT-WORK-AREA-ANCHOR  POINTER         +8 *
*                       RETURN-CODE                PIC S9(8) COMP  +12*
*                                                                     *
*   RETURN CODES:                                                     *
*        0   - SUCCESSFUL                                             *
*        4   - ENTITY ID NOT FOUND                                    *
*        8   - END-OF-BROWSE                                          *
*        16  - THREAD DDNAME TOKEN NOT FOUND                          *
*                                                                     *
***********************************************************************
*>>>>>>> up to here for conversion to single buffer
GENCKVRT ds    0H
*
         using saver,r13          map the save area
         STM   R14,R12,savgrs14   SAVE  CALLER'S  REGISTERS
*
         LArl  R12,gvbxrck        RE-ESTABLISH  PROGRAM BASE REGISTERS
         USING (GVBXRCK,code),R12
*
         lR    R9,R1              SAVE PARAMETER LIST ADDRESS

         srlg  r0,r0,32           move high half of r0 to low half
         srlg  r1,r1,32           move high half of r1 to low half
         srlg  r15,r15,32         move high half of r15 to low half
         lr    r2,r0              save high r0 in low r2
         lr    r3,r1              save high r1 in low r3
         lr    r4,r15             save high r15 in low r4

         L     R8,8(,R9)          LOAD WORK AREA ANCHOR ADDRESS
         LT    R1,0(,R8)          WORK AREA ALREADY   ALLOCATED ???
*
         BRP   GVBVR_15
*
         LHI   R0,epworklN+8       LOAD   WORK AREA SIZE
         STORAGE OBTAIN,LENGTH=(0),LOC=(ANY),CHECKZERO=YES GET WORKAREA
         if    cij,r15,ne,x'14'   not zeroed?
           LR  r10,r1             save address
           LR  R0,R1              ZERO  WORK  AREA
           LHI R1,epworkln+8
           SR  R14,R14
           SR  R15,R15
           MVCL R0,R14
           lr  r1,r10             restore pointer
         endif
*
         MVC   0(8,R1),CKVREYEB
         aghi  r1,l'cktkeyeb      move pointer past
         drop  r13
         USING epworkar,R1
         using savf5sa,epworkar
         stg   r13,savf5saprev    save current r13
         stmh  r2,r14,savf5sag64hs2    save high parts r2 - r14
         st    r2,savf5sag64hs0  save high parts of r0
         st    r3,savf5sag64hs1                      r1
         st    r4,savf5sag64hs15                and r15
         mvc   savf5said(4),=a(savf5said_value) set 'F5SA' in area
         lgr   R13,r1             Get new workarea into r13
         push  using
         drop  r1
         USING epworkar,R13
         using savf5sa,epworkar
         ST    R13,0(,R8)

         MVC   EPTOKNAM+0(8),GENEVA
         L     R14,0(,R9)
         MVC   EPTOKNAM+8(8),0(R14)
*
         CALL  IEANTRT,(TOKNLVL1,EPTOKNAM,EPTOKN,EPTOKNRC),            +
               MF=(E,EPREENT)
*
         L     R15,EPTOKNRC       SUCCESSFUL ???
         LTR   R15,R15
         BRZ   GVBVR_10           YES - CONTINUE
*
         LHI   R15,16             RETURN "TOKEN MISSING"
         BRU   GVBVR_99
*
GVBVR_10 L     R8,EPTOKNPA        LOAD "GVBXRCK" WORK AREA ADDRESS
         L     R8,PARTWORK-PARTAREA(,R8)
         USING XRCKWORK,R8
*
         ST    R8,EPXRCKWK        SAVE  ADDRESS
         BRU   GVBVR_20
*
         pop   using
GVBVR_15 ds    0h
         stg   r13,savf5saprev    save current r13
         stmh  r2,r14,savf5sag64hs2    save high parts r2 - r14
         st    r2,savf5sag64hs0  save high parts of r0
         st    r3,savf5sag64hs1                      r1
         st    r4,savf5sag64hs15                and r15
         llgtr R13,r1             Get new workarea into r13
         drop  r1
         USING epworkar,R13
         using savf5sa,epworkar
*
         L     R8,EPXRCKWK        LOAD  BASE REGISTER
         USING XRCKWORK,R8
*
GVBVR_20 L     R3,WKOPNBRK        INITIALIZE    BREAK  POINT
*
         L     R6,0(,R3)          INITIALIZE FILE  AREA ADDR "T" OPTION
         USING FILEAREA,R6
*
         LG    R7,WKBLKEND        COMPUTE LENGTH OF DATA IN BUFFER
         LG    R5,WKBLKBDW
         LGR   R14,R7
         SGR   R14,R5
*
         LGF   R2,4(,R9)          LOAD  GENERATED TRANSACTION REC ADDR
         LGH   R15,0(,R2)         LOAD  GENERATED RECORD   LENGTH
*
         lgr   R0,R7              BUFFER
         agr   r0,r15                    overflow ?
         CG    R0,WKBLKMAX
         BRH   GVBVR_25           YES
*
         LA    R0,0(R14,R15)      BLOCK  OVERFLOW ???
         Cfi   R0,BLKSIZE
         BRNH  GVBVR_80           NO  - CONTINUE
*
*
***********************************************************************
*  CLOSE OUT PREVIOUS BLOCK (BDW) AND START NEW BLOCK (BDW)           *
***********************************************************************
         Lgr   R15,R7             LOAD END-OF-DATA  ADDRESS
         Sgr   R15,R5             COMPUTE  BLOCK    LENGTH
         STH   R15,0(,R5)
*
         STG   R7,WKBLKBDW        START NEW  BDW
         XC    0(4,R7),0(R7)
         LA    R7,4(,R7)
         STG   R7,WKBLKEND
*
         if CG,R7,gt,WKBLKHI         NEW "HIGHWATER MARK" ???
           STG R7,WKBLKHI
         endif
*
         BRU   GVBVR_20           COPY  RECORD  INTO  NEW  BLOCK
*
*
***********************************************************************
*   IF THERE IS AN "OVERFLOW" OF "LR BUFFER EXTENSION" DISPLAY ERROR  *
***********************************************************************
GVBVR_25 ds    0h
*
         Llgt  R2,4(,R9)          LOAD  GENERATED TRANSACTION REC ADDR
*        Aghi  R2,4               SKIP  RDW
*
*
         GVBMSG FORMAT,MSGNO=CK_BUFFER_GEN_OVERFLOW,SUBNO=3,           +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(GENCKEPV,L'GENCKEPV),                             +
               SUB3=(4(,r2),24),             Gen record/key            +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
         LHI   R15,16
         BRU   GVBVR_99
*
GVBVR_80 STH   R0,0(,R5)          UPDATE "BDW"
*
         STG   R7,FILERECA
         STH   R15,FILERECL
         STH   R15,FILEMAXL
         agsi  FILEICNT,b001
*
         if Ltg,R14,WKRECPRV,p    CLEAR PREVIOUS LAST RECORD FLAG
           NI  2(R14),255-X'40'
         endif
         STg   R7,WKRECPRV        UPDATE  PREVIOUS RECORD ADDR
*
         L     R14,WKLSTCUR       ADD   RECORD ADDRESS TO LIST
         STG   R7,0(,R14)
         LA    R14,8(,R14)        ADVANCE LIST ADDRESS
         C     R14,WKLSTMAX       CHECK   FOR OVERFLOW
         BRL   GVBVR_81
*
         L     R14,WKBRKID
*       MVC   WKPRTTXT+41(11),0(R14)
*       OI    WKPRTTXT+41+10,X'F0'
*
         GVBMSG FORMAT,MSGNO=CK_REC_PTR_TABLE_OVERFLOW,SUBNO=2,        +
               SUB1=(LPGMNAME,L'LPGMNAME),                             +
               SUB2=(0(R14),11),             Gen record/key            +
               MSGBUFFER=(WKTXTBUF,L'WKTXTBUF),                        +
               MF=(E,WK_MSG)
*
         BRU   RTNERROR
*
GVBVR_81 ST    R14,WKLSTCUR
*
         LR    R1,R7
         LR    R14,R2
         LA    R15,255(,R15)
         LR    R0,R15
         SRL   R0,8
         BRU   GVBVR_85
GVBVR_82 MVC   0(256,R1),0(R14)   COPY  256 BYTES
         LA    R1,256(,R1)        ADVANCE  TARGET
         LA    R14,256(,R14)      ADVANCE  SOURCE
GVBVR_85 BRCT  R0,GVBVR_82
         exrl  R15,MVCR1R14       COPY  REMAINDER
*
         OI    2(R7),X'40'        UPDATE  LAST RECORD  FLAG
*
         SLL   R15,24             CLEAR HIGH ODER LENGTH BYTES
         SRL   R15,24
         LA    R0,1(R1,R15)       UPDATE  CURRENT ENDING ADDRESS
         STG   R0,WKBLKEND
*
         if CG,R0,gt,WKBLKHI         NEW "HIGHWATER MARK" ???
           STG R0,WKBLKHI
         endif
*
         SR    R15,R15
*
GVBVR_99 ST    R15,EPLASTRC       SAVE LAST RETURN CODE
*
         lmh   r2,r14,savf5sag64hs2    Restore high halves
         lg    r13,savf5saprev         restore caller save area
         L     R14,12(,R9)
         ST    R15,0(,R14)
         using saver,r13
         l     r14,savgrs14            restore caller's r14
         lm    r2,r12,savgrs2          restore caller's r2- r12
         BSM   0,R14              RETURN
*
         DROP  R6
         DROP  R8
         DROP  R13
*
static   loctr
         LTORG
code     loctr
*****
At_end   loctr  ,                 position buffer at end of module
static   loctr  ,
*

HEADERB  DC    CL133' '
HEADERD  DC    CL042'0-----------------------------------------'
         DC    CL047'-----------------------------------------------'
         DC    CL044'--------------------------'
HEADER01 DC    CL133'1                                   GVBXRCK'
HEADER02 DC    CL133'                                CONTROL REPORT'
HEADER03 DC    CL133' RUN      DATE:  YY/MM/DD'
HEADER04 DC    CL133' COMPLETE TIME:  HH:MM:SS'
HEADER05 DC    CL133' GVBXRCK PARAMETERS:'
HEADER06 DC    CL044'0ENTITY   DDNAME   *CATEGORIES* PART  TY DU ' @010
         DC  CL050'PL  KP KL BL TP   EP PP DB2 GDG ** DATASET TEMPLAT'
         DC    CL039'E **                       OUT CATEGORY'      @010
HEADER07 DC    CL044' -------- -------- ------------ ----- -- -- ' @010
         DC  CL050'--  -- -- -- --  --- -- --- --- ------------------'
         DC    CL039'-------------------------- ------------'      @010
HEADER08 DC    CL044' EEEEEEEE DDDDDDDD TTTTTTTTTTTT PP/PP  T  D ' @010
         DC  CL050'NN  NN NN NN NN   NN NN  X   GG XXXXXXXXXXXXXXXXXX'
         DC    CL039'XXXXXXXXXXXXXXXXXXXXXXXXXX '                  @010
HEADER09 DC    CL133' PARTITION XX SUMMARY TOTALS'
HEADER10 DC    CL038'0 DDNAME  ENTITY/SRC INPUT REC COUNT  '
         DC    CL014'INPUT LENGTH  '
         DC    CL081'    INPUT BYTES      DUPLICATE REC COUNT'
HEADER11 DC    CL038' -------- ---------- ---------------   '
         DC    CL014'------------  '
         DC    CL081'-------------------  -------------------'
HEADER12 DC    CL038' XXXXXXXX XXXXXXXXXX ZZ,ZZZ,ZZZ,ZZ9   '
         DC    CL014'   ZZ,ZZ9     '
         DC    CL081'ZZZ,ZZZ,ZZZ,ZZZ,ZZ9       ZZ,ZZZ,ZZZ,ZZ9'
HEADER13 DC    CL038'                     ---------------  '
         DC    CL014'              '
         DC    CL081'-------------------  -------------------'
HEADER14 DC    CL038'           SUBTOTALS ZZ,ZZZ,ZZZ,ZZ9   '
         DC    CL014'              '
         DC    CL081'ZZZ,ZZZ,ZZZ,ZZZ,ZZ9       ZZ,ZZZ,ZZZ,ZZ9'
HEADER15 DC    CL038' (L) LOOKUPS NOT FOUND  ZZ,ZZZ,ZZZ,ZZ9'
         DC    CL095' '
HEADER16 DC    CL133' GVBXRCK GRAND TOTALS '
HEADER17 DC    CL038'0                    INPUT REC COUNT  '
         DC    CL014'              '
         DC    CL081'    INPUT BYTES      DUPLICATE REC COUNT'
HEADER18 DC    CL038'                     ---------------  '
         DC    CL014'              '
         DC    CL081'-------------------  -------------------'
                        EJECT
*
*
MSGPFCT  DC    AL2(L'MSGPFCTT)
MSGPFCTT DC    C' ** GVBXRCK - Number of view partitions: xxxxxxxx'
*                                                                 @017I
*
***********************************************************************
*   PARAMETERS FOR CALLING GVBUTHDR                                   *
***********************************************************************
TYPEPGM  DC    C'Base Product'
TYPEPGMl DC    al2(typepgml-typepgm)
NAMEPGMx  DC    c'GVBXRCK'
namePGMxl DC    al2(namepgml-namepgm)
titpgm   dc    c'Common-Key Buffer Read Process'
titpgml  dc    al2(titpgml-titpgm)
rpt_95ctrl  dc c'GVBXRCK Control Report'
rpt_95ctrll dc al2(rpt_95ctrll-rpt_95ctrl)
rpt_dd95xc dc   cl8'MERGRPT'
VERSBLD  DC    CL8'&SYSPARM'
BUFLGTH  DC    A(L'BUFFER)
         ds    0h
                        EJECT
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
*
*
GVBXRCK  CSECT
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
         END
