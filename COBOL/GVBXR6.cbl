           PROCESS RENT                                                         
           PROCESS NODYNAM                                                      
           PROCESS RMODE(AUTO)                                                  
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.    GVBXR6.                                                   
      *****************************************************************
      *                                                               *
      * (C) COPYRIGHT IBM CORPORATION 2023.                           *
      *     Copyright Contributors to the GenevaERS Project.          *
      * SPDX-License-Identifier: Apache-2.0                           *
      *                                                               *
      *****************************************************************
      *                                                               *
      * Licensed under the Apache License,                            *
      * Version 2.0 (the "License");                                  *
      * you may not use this file except in                           *
      * compliance with the License.                                  *
      * You may obtain a copy of the License at                       *
      *                                                               *
      *     http://www.apache.org/licenses/LICENSE-2.0                *
      *                                                               *
      *  Unless required by applicable law or                         *
      *  agreed to in writing, software                               *
      *  distributed under the License is distributed                 *
      *  on an "AS IS" BASIS,                                         *
      *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express *
      *  or implied.                                                  *
      *  See the License for the specific language governing          *
      *  permissions and limitations under the License.               *
      *                                                               *
      *                     G V B X R 6                               *         
      *                                                               *         
      *         GENEVA READ EXIT FOR CUSTNAME FILE                    *         
      *                                                               *         
      *  PURPOSE:   THIS PROGRAM IS A GENEVA READ EXIT.               *         
      *                                                               *         
      *   INPUTS:   1. QSAM CUSTNAME FILE      (DDNAME=CUSTNAMS)      *         
      *                                                               *         
      *   OUTPUTS:  1. NONE                                           *         
      *                                                               *         
      *   PROCESS:                                                    *         
      *    THE PROGRAM IS INVOKED BY GENEVA AS A READ EXIT AND IS     *         
      *    EXECUTED IN A MULTI-THREAD ENVIRONMENT.                    *         
      *                                                               *         
      *                                                               *         
      *    IMPORTANT: PROGRAM MUST HAVE RES, RENT IN COMPILE          *         
      *               PROCESS OPTIONS AND RENT IN LINK FOR EXECUTION  *         
      *               IN LE 370 MULTI-THREAD ENVIRONMENT.             *         
      *                                                               *         
      *   CALLED PROGRAMS:                                            *         
      *    GVBUR05  - GENEVA OBTAIN STORAGE IN MEMORY                 *         
      *    GVBTP90  - FILE I-O PROCESSING                             *         
      *    GVBUR66  - ENQ-DEQ PROCESSING                              *         
      *                                                               *         
      *****************************************************************         
      *                                                                         
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       INPUT-OUTPUT SECTION.                                                    
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  FILLER                       PIC X(40)  VALUE                        
           'WORKING STORAGE FOR GVBXR6 STARTS HERE'.                            
      *                                                                         
      *****************************************************************         
      *             S W I T C H E S                                   *         
      *****************************************************************         
                                                                                
       01  WS-FIRST-TIME-SW             PIC X(01)  VALUE 'Y'.                   
           88  WS-FIRST-TIME                       VALUE 'Y'.                   
      *                                                                         
       01  WS-TRACE-TIME-SW             PIC S9(08) COMP VALUE +0.               
      *                                                                         
      * SHOULD THIS BE 'Y' AND 'N' ?                                            
      *                                                                         
       01  WS-FNCL-EOF-SW               PIC X(01)  VALUE 'N'.                   
           88  WS-FNCL-EOF                         VALUE 'Y'.                   
      *****************************************************************         
      *             C O N S T A N T S                                 *         
      *****************************************************************         
                                                                                
       01  WS-ABEND-CD                  PIC X(04)  VALUE X'0016'.               
       01  WS-EOF-IND                   PIC S9(08) COMP   VALUE +8.             
       01  WS-PRCS-CD                   PIC X(01)  VALUE 'D'.                   
       01  WS-DDNAME-LB949              PIC X(08)  VALUE '        '.            
       01  WS-GVBUR05                   PIC X(08)  VALUE 'GVBUR05 '.            
       01  WS-GVBTP90                   PIC X(08)  VALUE 'GVBTP90 '.            
       01  WS-GVBUR66                   PIC X(08)  VALUE 'GVBUR66 '.            
      *                  FOR ERROR MESSAGING                                    
       01  MODNAME                      PIC  X(08) VALUE 'GVBXR6  '.            
                                                                                
      *****************************************************************         
      *             C O U N T E R S                                   *         
      *****************************************************************         
                                                                                
       01  WS-FNCL-RCRDS-READ           PIC S9(11) COMP-3 VALUE +0.             
      *                                                                         
      *****************************************************************         
      *       F I L L   T H E   G E N E V A   B U F F E R                       
      *                                                                         
      *  BUFFER REFERS TO THE BUFFER THAT GENEVA USES TO PASS DATA              
      *  TO THE VIEWS.  THESE VARIABLES AID COMMUNICATION WITH                  
      *  GENEVA.                                                                
      *                                                                         
      *****************************************************************         
      *                                                                         
       01  WS-TB-STRT-ENTRY             PIC S9(04) COMP VALUE +1.               
       01  WS-TB-INCREMENT              PIC S9(04) COMP VALUE +0.               
       01  WS-TB-ROWS-PER-BUFFER        PIC S9(04) COMP VALUE +0.               
       01  WS-TB-ROWS-REMAINING         PIC S9(04) COMP VALUE +0.               
      *                                                                         
      *                                                                         
      *****************************************************************         
      *                P O I N T E R S                                          
      *****************************************************************         
                                                                                
       01  WS-TP90-INPT-PTR             POINTER.                                
       01  WS-WORKAREA-ANCHOR           POINTER.                                
                                                                                
      *****************************************************************         
      *             V A R I A B L E S                                 *         
      *****************************************************************         
                                                                                
       01  WS-DISPLAY-MASK-1            PIC ZZ,ZZZ,ZZZ,ZZ9.                     
      *                                                                         
      *****************************************************************         
      *           O T H E R   V A R I A B L E S                                 
      *****************************************************************         
      *                      LENGTH OF STORAGE AREA                             
       01  WS-WORK-AREA-LNGTH           PIC S9(08) COMP.                        
       01  WS-GLOBAL-WORKAREA-SIZE      PIC S9(08) COMP.                        
       01  WS-CURNT-TSTMP               PIC X(26)  VALUE SPACES.                
       01  WS-MORE-TERM-FLAG            PIC X(1)   VALUE 'N'.                   
       01  WS-33509-FOUND               PIC X(1)   VALUE 'N'.                   
      *B11                                                                      
       01  WS-33509-RISK-FOUND          PIC X(1)   VALUE 'N'.                   
      *B11                                                                      
       01  WS-TEMP-PLCY-TERM-EFF-DT     PIC X(8)   VALUE SPACES.                
       01  WS-TEMP-AGRE-BUSN-ID         PIC S9(11) COMP-3 VALUE ZERO.           
      *                                                                         
      *                                                                         
       01  GVBTP90-RECORD-AREA          PIC X(96).                              
      *                                                                         
      *****************************************************************         
      *  GVBUR66 - ENQ/DEQ PARAMETERS                                           
      *****************************************************************         
       01 ENQ-DEQ-PARMS-TOKEN.                                                  
         05  ENQ-DEQ-FUNC                PIC X(3).                              
         05  ENQ-DEQ-CTRL                PIC X(1)   VALUE 'E'.                  
         05  ENQ-DEQ-RNAME               PIC X(8)   VALUE 'GENEVA'.             
         05  ENQ-DEQ-QNAME               PIC X(128) VALUE 'GVBXRG'.             
         05  ENQ-SCOPE-REQUEST           PIC X(1)   VALUE '1'.                  
         05  ENQ-DEQ-FILLER              PIC X(3)   VALUE SPACES.               
      *                                                                         
      *****************************************************************         
      *  IEANXX  - NAME TOKEN SERVICES                                          
      *****************************************************************         
       01 NAME-TOKEN-AREA.                                                      
         05 WS-TOKEN-NAME.                                                      
            10 WS-TOKEN-GENEVA        PIC  X(08).                               
            10 WS-TOKEN-PGM-NAME      PIC  X(08).                               
         05 WS-TOKEN-VALUE.                                                     
            10 WS-TKN-SHARED-PTR      POINTER.                                  
            10 FILLER                 POINTER.                                  
            10 FILLER                 POINTER.                                  
            10 FILLER                 POINTER.                                  
         05 WS-TOKEN-LEVEL            PIC S9(08)    COMP.                       
         05 WS-TOKEN-PERSISTENCE      PIC S9(08)    COMP.                       
         05 WS-TOKEN-RTRN-CD          PIC S9(08)    COMP.                       
      *                                                                         
      *****************************************************************         
      *  GVBTP90 - I/O COMMUNICATION WITH OPERATING SYSTEM                      
      *****************************************************************         
       01  GVBTP90-RECORD-KEY           PIC X(01).                              
      *                                                                         
       01  GVBTP90-INFO-RETURN-DATA.                                            
           05  GVBTP90-KEY-OFFSET       PIC S9(08) COMP VALUE ZEROES.           
           05  GVBTP90-KEY-LENGTH       PIC S9(08) COMP VALUE ZEROES.           
           05  GVBTP90-MAX-RECLEN       PIC S9(08) COMP VALUE ZEROES.           
           05  GVBTP90-NUM-RECORDS      PIC S9(08) COMP VALUE ZEROES.           
           05  FILLER                   PIC X(4230)     VALUE SPACES.           
      *                                                                         
       01  GVBTP90-INPUT-RECORD-LENGTH  PIC S9(04) COMP VALUE ZEROES.           
       01  GVBTP90-FILE-RECORD-LENGTH   PIC S9(04) COMP VALUE ZEROES.           
       01  GVBTP90-INPUT-RECORD-FMT     PIC X(01)       VALUE SPACES.           
      *                                                                         
       01  GVBTP90-MAX-FB-RECORD-LENGTH PIC S9(04) COMP VALUE +4240.            
       01  GVBTP90-MAX-VB-RECORD-LENGTH PIC S9(04) COMP VALUE +4244.            
      *                                                                         
       01  GVBTP90-KEY                  PIC X(80)     VALUE SPACES.             
      *                                                                         
       01  GVBTP90-FUNCTION-CODES.                                              
           05  GVBTP90-VALUE-CLOSE      PIC X(02) VALUE 'CL'.                   
           05  GVBTP90-VALUE-DELETE     PIC X(02) VALUE 'DL'.                   
           05  GVBTP90-VALUE-INFO       PIC X(02) VALUE 'IN'.                   
           05  GVBTP90-VALUE-LOCATE     PIC X(02) VALUE 'LO'.                   
           05  GVBTP90-VALUE-OPEN       PIC X(02) VALUE 'OP'.                   
           05  GVBTP90-VALUE-READ       PIC X(02) VALUE 'RD'.                   
           05  GVBTP90-VALUE-READNEXT   PIC X(02) VALUE 'BR'.                   
           05  GVBTP90-VALUE-START-BROWSE                                       
                                        PIC X(02) VALUE 'SB'.                   
           05  GVBTP90-VALUE-UPDATE     PIC X(02) VALUE 'UP'.                   
           05  GVBTP90-VALUE-WRITE      PIC X(02) VALUE 'WR'.                   
           05  GVBTP90-VALUE-RELEASE    PIC X(02) VALUE 'RI'.                   
      *                                                                         
       01  GVBTP90-FILE-TYPES.                                                  
           05  GVBTP90-VALUE-SEQUENTIAL PIC X(01) VALUE 'S'.                    
           05  GVBTP90-VALUE-VSAM       PIC X(01) VALUE 'V'.                    
      *                                                                         
       01  GVBTP90-FILE-MODES.                                                  
           05  GVBTP90-VALUE-INPUT      PIC X(02) VALUE 'I '.                   
           05  GVBTP90-VALUE-OUTPUT     PIC X(02) VALUE 'O '.                   
           05  GVBTP90-VALUE-IO         PIC X(02) VALUE 'IO'.                   
      *                                                                         
       01  GVBTP90-RETURN-CODES.                                                
           05  GVBTP90-VALUE-SUCCESSFUL PIC X(01) VALUE '0'.                    
           05  GVBTP90-VALUE-NOT-FOUND  PIC X(01) VALUE '1'.                    
           05  GVBTP90-VALUE-END-OF-FILE                                        
                                        PIC X(01) VALUE '2'.                    
           05  GVBTP90-VALUE-BAD-PARAMETER                                      
                                        PIC X(01) VALUE 'B'.                    
           05  GVBTP90-VALUE-IO-ERROR   PIC X(01) VALUE 'E'.                    
           05  GVBTP90-VALUE-LOGIC-ERROR                                        
                                        PIC X(01) VALUE 'L'.                    
      *                                                                         
       01  GVBTP90-RECORD-FORMATS.                                              
           05  GVBTP90-VALUE-FIXED-LEN  PIC  X(01) VALUE 'F'.                   
           05  GVBTP90-VALUE-VARIABLE-LEN                                       
                                        PIC  X(01) VALUE 'V'.                   
      *****************************************************************         
      *             T A B L E S                                       *         
      *****************************************************************         
      *                                                                         
      *    BLOCK OF LB952 RECORDS TABLE                                         
       01  WS-TB-TBL-MAX-ENTRIES        PIC S9(08) COMP VALUE +100.             
       01  WS-TB-TBL-ENTRIES            PIC S9(08) COMP VALUE +0.               
       01  WS-FNCL-BLK-TBL.                                                     
           05  WS-TB-TBL-ENTRY          OCCURS 100      TIMES                   
                                        PIC X(96).                              
      *                                                                         
       01  FILLER                       PIC X(40)       VALUE                   
           'WORKING STORAGE FOR GVBXR6 ENDS HERE'.                              
                                                                                
       EJECT                                                                    
       LINKAGE SECTION.                                                         
                                                                                
      *** THIS IS A COPY OF GVBX95PC ***                                        
           COPY GVBX95PC.                                                       
                                                                                
      *****************************************************************         
      *          INPUT RECORD LAYOUTS                                 *         
      *****************************************************************         
                                                                                
      *------------------------------------------------------------             
      *                                                                         
      *             STORAGE FOR I-O PROGRAM GVBTP90                             
      *------------------------------------------------------------             
       01 GVBTP90-PARAMETER-AREA-INPT.                                          
          05  GVBTP90-ANCHOR            POINTER.                                
          05  GVBTP90-DDNAME            PIC  X(08).                             
          05  GVBTP90-FUNCTION-CODE     PIC  X(02).                             
          05  GVBTP90-FILE-TYPE         PIC  X(01).                             
          05  GVBTP90-FILE-MODE         PIC  X(02).                             
          05  GVBTP90-RETURN-CODE       PIC  X(01).                             
          05  GVBTP90-VSAM-RETURN-CODE  PIC S9(04)  COMP.                       
          05  GVBTP90-RECORD-LENGTH     PIC S9(04)  COMP.                       
          05  GVBTP90-RECFM             PIC  X(01).                             
          05  WS-EVENT-DDNAME.                                                  
              10  WS-EVENT-DDNAME-1ST-3 PIC  X(03).                             
              10  WS-EVENT-DDNAME-4-6   PIC  X(03).                             
              10  WS-EVENT-DDNAME-LAST-2                                        
                                        PIC  X(02).                             
      *------------------------------------------------------------             
      *                                                                         
      *             STORAGE FOR GLOBAL WORKAREA                                 
      *------------------------------------------------------------             
       01 LS-GLOBAL-WORKAREA.                                                   
         02 LS-RECORDS-READ             PIC S9(08)  COMP.                       
         02 LS-PARTITIONS-PROCESSED     PIC S9(08)  COMP.                       
         02 LS-PARTITIONS-TOTAL         PIC S9(08)  COMP.                       
      *                                                                         
      *                                                                         
      ******************                                                        
      *****************************************************************         
      * MAIN LOGIC.                                                   *         
      *    THE INPUT DATA IS READ THROUGH GENEVA VIEW                 *         
      *                                                               *         
      *    WHEN WRITING OUT MULTIPLE DATA BLOCKS, CONTROL IS PASSED   *         
      *    TO GENEVA TO WRITE THE BUFFER AND CONTROL WILL BE          *         
      *    RETURNED BACK TO THIS PROGRAM AT THE TOP.  SO, IT IS       *         
      *    NECESSARY TO UNLOAD THE TABLE AT THE START OF THE          *         
      *    PROCEDURE DIVISION UNTIL ALL ENTRIES ARE COMPLETELY        *         
      *    WRITTEN OUT BEFORE PROCESSING THE NEXT INPUT.              *         
      *    THERE SHOULD BE NO ROWS TO UNLOAD THE FIRST TIME THIS      *         
      *    PROGRAM IS CALLED.                                         *         
      *                                                               *         
      *    THE INPUT DATA IS READ, RECORD BY RECORD, UNTIL            *         
      *    END OF FILE OR THE TABLE OF OUTPUT DATA HAS BEEN FILLED.   *         
      *    THEN THE UNLOADING OF THIS TABLE WOULD OCCUR AGAIN.        *         
      *    SINCE WE CAN GENERATE UP TO 3 OUTPUT LB949 RCRDS FOR ONE   *         
      *    INPUT LB949 RCRD,THE 200- LOOP CHECKS IF IT HAS ROOM IN THE*         
      *    TBL FOR 3 MORE RCRDS BEFORE PROCESSING THE NEXT INPUT RCRD.*         
      *                                                               *         
      *    AT END OF EACH INPUT FILE, TOTALS ARE DISPLAYED AND FILES  *         
      *    CLOSED.                                                    *         
      *****************************************************************         
                                                                                
       PROCEDURE DIVISION USING X95PARM1-ENV-DATA                               
                                X95PARM2-EVENT-FILE-DATA                        
                                X95PARM3-STARTUP-DATA                           
                                X95PARM4-EVENT-REC-PTR                          
                                X95PARM5-EXTRACT-REC                            
                                X95PARM6-LOOKUP-KEY                             
                                X95PARM7-WORK-AREA-ANCHOR                       
                                X95PARM8-RETURN-CODE                            
                                X95PARM9-RESULT-PTR                             
                                X95PARMA-RESULT-BLOCK-SIZE.                     
                                                                                
       000-MAIN-LOGIC.                                                          
                                                                                
           DISPLAY 'GVBXR6: WS-FNCL-EOF-SW          = '                         
                      WS-FNCL-EOF-SW                                            
           DISPLAY 'GVBXR6: X95PARM1-PHASE-CODE     = '                         
                      X95PARM1-PHASE-CODE                                       
           DISPLAY 'GVBXR6: X95PARM1-PARTITION-COUNT= '                         
                      X95PARM1-PARTITION-COUNT                                  
           DISPLAY 'GVBXR6: X95PARM2-EVENT-DDNAME   = '                         
                      X95PARM2-EVENT-DDNAME                                     
           DISPLAY 'GVBXR6: WS-FIRST-TIME-SW        = '                         
                      WS-FIRST-TIME-SW                                          
      *OPEN PHASE                                                               
           IF   X95PARM1-OPEN-PHASE                                             
             DISPLAY 'GVBXR6: OPEN PHASE'                                       
             DISPLAY 'GVBXR6: THREAD-NBR = ' X95PARM1-THREAD-NBR                
             DISPLAY 'GVBXR6: CURRENT-VIEW-ID = '                               
                      X95PARM1-CURRENT-VIEW-ID                                  
             DISPLAY 'GVBXR6: PROCESS DATE AND TIME: '                          
                      X95PARM1-PROCESS-DATE ' ' X95PARM1-PROCESS-TIME           
      *                                                                         
      * NEEDED IF PARTITIONS EXECUTE IN SAME THREAD/LE ENCLAVE                  
      * ******************************************************                  
      *                                                                         
             MOVE 'Y'                   TO WS-FIRST-TIME-SW                     
             MOVE 'N'                   TO WS-FNCL-EOF-SW                       
             MOVE +0                    TO WS-FNCL-RCRDS-READ                   
      *                                                                         
      * *****************************************************                   
      *                                                                         
             MOVE ZERO               TO X95PARM8-RETURN-CODE                    
                                        RETURN-CODE                             
           END-IF                                                               
                                                                                
      *CLOSE PHASE                                                              
           IF   X95PARM1-CLOSE-PHASE                                            
             DISPLAY 'GVBXR6: CLOSE PHASE'                                      
             MOVE ZERO               TO X95PARM8-RETURN-CODE                    
                                        RETURN-CODE                             
             PERFORM 9900-FINALIZATION                                          
             GOBACK                                                             
           END-IF                                                               
                                                                                
      *FIRST TIME READ                                                          
           IF WS-FIRST-TIME                                                     
             DISPLAY 'GVBXR6: FIRST TIME'                                       
             PERFORM 100-INIT               THRU 100-EXIT                       
           ELSE                                                                 
             IF WS-TRACE-TIME-SW = 1                                            
               DISPLAY 'GVBXR6: SUBSEQUENT ' X95PARM1-CURRENT-VIEW-ID           
             ELSE                                                               
               DISPLAY 'GVBXR6:'                                                
             END-IF                                                             
           END-IF                                                               
                                                                                
           ADD +1 TO WS-TRACE-TIME-SW                                           
                                                                                
      *  UNLOAD LB952/LB949/EANNY RECORDS TO GENEVA                             
           DISPLAY 'GVBXR6: FIRST 600-UNLD-FNCL-BLK-TBL '                       
                   'WS-TB-ROWS-REMAINING = ' WS-TB-ROWS-REMAINING               
           PERFORM 600-UNLD-FNCL-BLK-TBL                                        
                   UNTIL  WS-TB-ROWS-REMAINING NOT GREATER +0                   
                                                                                
           MOVE +1                    TO WS-TB-STRT-ENTRY                       
           MOVE +0                    TO WS-TB-INCREMENT                        
           MOVE +0                    TO WS-TB-TBL-ENTRIES                      
      *                                                                         
      *  READ LB949 RECORDS & SEND FOR ANDREW PROCESS/                          
      *  TRANSLATION/ADVANCE RECORD CREATION/PEND CREATION                      
           DISPLAY 'GVBXR6: 200-FNCL-RCRD-CNTL ===>>> '                         
                   'WS-TB-ROWS-REMAINING = ' WS-TB-ROWS-REMAINING               
      *                                                                         
           PERFORM 200-FNCL-RCRD-CNTL        THRU 200-EXIT                      
             UNTIL  WS-FNCL-EOF OR                                              
                    WS-TB-TBL-ENTRIES >= WS-TB-TBL-MAX-ENTRIES                  
      *             WS-TB-TBL-ENTRIES >= WS-TB-TBL-MAX-ENTRIES - 3              
      *                                                                         
           MOVE WS-TB-TBL-ENTRIES TO WS-TB-ROWS-REMAINING                       
      *                                                                         
           DISPLAY 'GVBXR6: 200-FNCL-RCRD-CNTL <<<==='                          
                   'WS-TB-ROWS-REMAINING = ' WS-TB-ROWS-REMAINING               
      *                                                                         
      *  UNLOAD LB952/LB949/EANNY RECORDS TO GENEVA                             
           DISPLAY 'GVBXR6: SECOND 600-UNLD-FNCL-BLK-TBL'                       
                   ' WS-TB-ROWS-REMAINING = ' WS-TB-ROWS-REMAINING              
           PERFORM 600-UNLD-FNCL-BLK-TBL                                        
                   UNTIL  WS-TB-ROWS-REMAINING NOT GREATER +0                   
                                                                                
      *  EOF LB949 INPUT FILE                                                   
           IF WS-FNCL-EOF                                                       
              MOVE WS-EOF-IND         TO X95PARM8-RETURN-CODE                   
                                         RETURN-CODE                            
              GOBACK                                                            
           ELSE                                                                 
             MOVE ZERO               TO X95PARM8-RETURN-CODE                    
                                        RETURN-CODE                             
           END-IF                                                               
           .                                                                    
      *                                                                         
       000-EXIT.                                                                
           EXIT.                                                                
      *                                                                         
      *                                                                         
      ******************************************************************        
      *  PROGRAM INITIALIZATIONS:                                      *        
      *  - DETERMINE NUMBER OF POSSIBLE ROWS THAT CAN BE WRITTEN TO    *        
      *    BUFFER                                                      *        
      *  - ACQUIRE STORAGE FOR LB949 BUFFER                           *         
      *  - CALL GVBTP90 FOR OPENING THE INPUT FILE THROUGH GENEVA     *         
      ******************************************************************        
       100-INIT.                                                                
                                                                                
           DISPLAY 'GVBXR6: 100-INIT'                                           
      *                                                                         
      *  BUFFER DETERMINATION                                                   
           MOVE LENGTH OF WS-TB-TBL-ENTRY                                       
                                      TO X95PARM2-EVENT-REC-LEN                 
                                         X95PARM2-MAX-REC-LEN                   
           SET X95PARM2-FIXED-LENGTH  TO TRUE                                   
      *                                                                         
           MOVE LOW-VALUES            TO X95PARM2-REC-FLD-DELIM                 
      *                                                                         
      *    COMPUTE WS-TB-ROWS-PER-BUFFER                                        
      *            = 32760  /  LENGTH OF WS-TB-TBL-ENTRY                        
      *    COMPUTE X95PARM2-MAX-BLOCK-SIZE =                                    
      *            WS-TB-ROWS-PER-BUFFER *                                      
      *                        LENGTH OF WS-TB-TBL-ENTRY                        
      *    END-COMPUTE                                                          
      *                                                                         
      *                                                                         
           MOVE   +100                TO WS-TB-ROWS-PER-BUFFER                  
      *                                                                         
           COMPUTE X95PARM2-MAX-BLOCK-SIZE =                                    
                             WS-TB-ROWS-PER-BUFFER *                            
                               LENGTH OF WS-TB-TBL-ENTRY                        
           END-COMPUTE                                                          
      *                                                                         
           MOVE X95PARM2-EVENT-DDNAME TO WS-DDNAME-LB949                        
      *                                                                         
           MOVE 'N'                   TO WS-FIRST-TIME-SW                       
      *                                                                         
           DISPLAY 'GVBXR6: X95PARM2-EVENT-REC-LEN  = '                         
                      X95PARM2-EVENT-REC-LEN                                    
           DISPLAY 'GVBXR6: X95PARM2-MAC-REC-LEN    = '                         
                      X95PARM2-MAX-REC-LEN                                      
           DISPLAY 'GVBXR6: X95PARM2-MAX-BLOCK-SIZE = '                         
                      X95PARM2-MAX-BLOCK-SIZE                                   
           DISPLAY 'GVBXR6: X95PARM2-EVENT-DDNAME   = '                         
                      X95PARM2-EVENT-DDNAME                                     
           DISPLAY 'GVBXR6: WS-FNCL-EOF-SW          = '                         
                      WS-FNCL-EOF-SW                                            
      *                                                                         
      *   ACQUIRE STORAGE FOR TP90                                              
      *                                                                         
           MOVE LENGTH               OF GVBTP90-PARAMETER-AREA-INPT             
                                     TO WS-WORK-AREA-LNGTH                      
                                                                                
           CALL WS-GVBUR05 USING     WS-TP90-INPT-PTR                           
                                     WS-WORK-AREA-LNGTH                         
           END-CALL                                                             
                                                                                
           SET ADDRESS                OF GVBTP90-PARAMETER-AREA-INPT            
                                      TO WS-TP90-INPT-PTR                       
           SET X95PARM7-WORK-AREA-ANCHOR                                        
                                      TO WS-TP90-INPT-PTR                       
      *                                                                         
           DISPLAY 'GVBXR6: ' WS-WORK-AREA-LNGTH                                
                   ' BYTES OF MEMORY ACQUIRED FOR TP90'                         
      *                                                                         
      *   PROCESS THE INPUT FILE FOR OPEN                                       
      *                                                                         
           MOVE  WS-DDNAME-LB949       TO GVBTP90-DDNAME                        
           MOVE GVBTP90-VALUE-OPEN     TO GVBTP90-FUNCTION-CODE                 
                                                                                
           PERFORM 0900-LB949-INPT-FILE                                         
      *                                                                         
      *   GET GLOBAL STORAGE IF NOT ALREADY DONE                                
      *                                                                         
           MOVE 'ENQ' TO ENQ-DEQ-FUNC OF ENQ-DEQ-PARMS-TOKEN                    
           CALL WS-GVBUR66 USING ENQ-DEQ-PARMS-TOKEN                            
                                                                                
           MOVE 'GENEVA'   TO   WS-TOKEN-GENEVA                                 
           MOVE 'GVBXRGB'  TO   WS-TOKEN-PGM-NAME                               
           MOVE +2         TO   WS-TOKEN-LEVEL                                  
           MOVE ZERO       TO   WS-TOKEN-PERSISTENCE                            
           MOVE ZERO       TO   WS-TOKEN-RTRN-CD                                
                                                                                
           CALL 'IEANTRT'  USING WS-TOKEN-LEVEL                                 
                                 WS-TOKEN-NAME                                  
                                 WS-TOKEN-VALUE                                 
                                 WS-TOKEN-RTRN-CD                               
                                                                                
           IF WS-TOKEN-RTRN-CD NOT = ZERO                                       
                                                                                
             MOVE LENGTH OF LS-GLOBAL-WORKAREA                                  
               TO WS-GLOBAL-WORKAREA-SIZE                                       
                                                                                
             CALL WS-GVBUR05 USING WS-TKN-SHARED-PTR                            
                                   WS-GLOBAL-WORKAREA-SIZE                      
                                                                                
             SET  ADDRESS OF LS-GLOBAL-WORKAREA                                 
              TO  WS-TKN-SHARED-PTR                                             
      *                                                                         
             CALL 'IEANTCR'    USING WS-TOKEN-LEVEL                             
                                     WS-TOKEN-NAME                              
                                     WS-TOKEN-VALUE                             
                                     WS-TOKEN-PERSISTENCE                       
                                     WS-TOKEN-RTRN-CD                           
                                                                                
             IF WS-TOKEN-RTRN-CD NOT = ZERO                                     
                 DISPLAY ' '                                                    
                 DISPLAY                                                        
                   'GVBXR6: UNABLE TO CREATE NAME/TOKEN, RC: '                  
                                     WS-TOKEN-RTRN-CD                           
                 STOP 666                                                       
             ELSE                                                               
                 MOVE +0 TO LS-RECORDS-READ                                     
                 MOVE +0 TO LS-PARTITIONS-PROCESSED                             
                 MOVE X95PARM1-PARTITION-COUNT TO                               
                      LS-PARTITIONS-TOTAL                                       
      *                                                                         
                 DISPLAY 'GVBXR6: GLOBAL WORKAREA ALLOCATED BY THREAD '         
                         X95PARM1-THREAD-NBR                                    
                 DISPLAY 'GVBXR6: TOTAL PARTITIONS PRESENT '                    
                        LS-PARTITIONS-TOTAL                                     
             END-IF                                                             
           ELSE                                                                 
             SET ADDRESS OF LS-GLOBAL-WORKAREA                                  
              TO  WS-TKN-SHARED-PTR                                             
           END-IF                                                               
      *                                                                         
           MOVE 'DEQ' TO ENQ-DEQ-FUNC OF ENQ-DEQ-PARMS-TOKEN                    
           CALL WS-GVBUR66 USING ENQ-DEQ-PARMS-TOKEN                            
           .                                                                    
                                                                                
       100-EXIT.                                                                
           EXIT.                                                                
      *                                                                         
      *                                                                         
      ******************************************************************        
      *  MOVE EACH CUSTNAME RECORD TO OUTPUT BLOCK UNTIL EOF OR TABLE  *        
      *  IS FULL.                                                      *        
      ******************************************************************        
      *                                                                         
       200-FNCL-RCRD-CNTL.                                                      
                                                                                
           MOVE WS-DDNAME-LB949        TO GVBTP90-DDNAME                        
      *                                                                         
           MOVE GVBTP90-VALUE-READ                                              
              TO GVBTP90-FUNCTION-CODE                                          
      *                                                                         
           PERFORM 0900-LB949-INPT-FILE                                         
      *                                                                         
           EVALUATE TRUE                                                        
      *                                 *** SUCCESSFUL READ ***                 
             WHEN GVBTP90-RETURN-CODE   =                                       
                   GVBTP90-VALUE-SUCCESSFUL                                     
      *                                                                         
                ADD  +1                TO WS-FNCL-RCRDS-READ                    
                ADD  +1                TO WS-TB-TBL-ENTRIES                     
                MOVE GVBTP90-RECORD-AREA                                        
                                   TO WS-TB-TBL-ENTRY(WS-TB-TBL-ENTRIES)        
      *                                                                         
      *                                 *** END OF FILE PROCESS ***             
             WHEN GVBTP90-RETURN-CODE =                                         
                   GVBTP90-VALUE-END-OF-FILE                                    
      *                                                                         
                 DISPLAY 'GVBXR6: END OF FILE REACHED -----------------'        
                 MOVE  'Y'             TO WS-FNCL-EOF-SW                        
      *                                 *** EMPTY INPUT FILE ***                
                 IF WS-FNCL-RCRDS-READ = ZEROES                                 
                   PERFORM 9920-EMPTY-FILE                                      
                   SET X95PARM8-END-OF-FILE TO TRUE                             
                 ELSE                                                           
                   PERFORM 9910-NON-EMPTY-FILE                                  
                 END-IF                                                         
      *                                                                         
             WHEN OTHER                                                         
                 DISPLAY 'GVBXR6: ' ' '                                         
                 DISPLAY 'GVBXR6: '                                             
                     X95PARM2-EVENT-DDNAME '  '                                 
                  GVBTP90-DDNAME                                                
                  '  GVBXR6-0200, GVBTP90 READ INPUT FAILED'                    
                 PERFORM 9998-TP90-ERR                                          
           END-EVALUATE                                                         
           .                                                                    
                                                                                
       200-EXIT.                                                                
           EXIT.                                                                
      *                                                                         
      *                                                                         
      ******************************************************************        
      *  UNLOAD BLOCK OF CUSTNAME RECORDS TO GENEVA.                   *        
      *  IF ROWS REMAINING IN OUTPUT TABLE IS GREATER THAN THE BUFFER, *        
      *  THEN IT IS A FULL BLOCK BEING RETURNED. OTHERWISE, IT IS A    *        
      *  PARTIAL BLOCK BEING RETURNED.                                 *        
      ******************************************************************        
       600-UNLD-FNCL-BLK-TBL.                                                   
                                                                                
      *                                                                         
           ADD WS-TB-INCREMENT            TO WS-TB-STRT-ENTRY                   
                                                                                
           SET X95PARM9-RESULT-PTR-LOW    TO                                    
                       ADDRESS OF WS-TB-TBL-ENTRY (WS-TB-STRT-ENTRY)            
           MOVE ZERO TO X95PARM9-RESULT-PTR-NUMERICH                            
      *                                                                         
      * **************     FULL BLOCK        ******************                 
                                                                                
           IF WS-TB-ROWS-REMAINING GREATER WS-TB-ROWS-PER-BUFFER                
              COMPUTE X95PARMA-RESULT-BLOCK-SIZE =                              
                 (WS-TB-ROWS-PER-BUFFER * LENGTH OF WS-TB-TBL-ENTRY)            
              END-COMPUTE                                                       
              SUBTRACT WS-TB-ROWS-PER-BUFFER                                    
                  FROM WS-TB-ROWS-REMAINING                                     
              MOVE WS-TB-ROWS-PER-BUFFER TO WS-TB-INCREMENT                     
              DISPLAY 'GVBXR6: RETURNING FULL BLOCK'                            
      *                                                                         
      * **************     PARTIAL BLOCK     ******************                 
           ELSE                                                                 
      *                                                                         
              COMPUTE X95PARMA-RESULT-BLOCK-SIZE =                              
                 (WS-TB-ROWS-REMAINING  * LENGTH OF WS-TB-TBL-ENTRY)            
              END-COMPUTE                                                       
              MOVE +0                     TO WS-TB-ROWS-REMAINING               
                                                WS-TB-INCREMENT                 
              MOVE +1                     TO WS-TB-STRT-ENTRY                   
              DISPLAY 'GVBXR6: RETURNING PART BLOCK'                            
           END-IF                                                               
                                                                                
           SET X95PARM8-SUCCESSFUL  TO TRUE                                     
           GOBACK                                                               
           .                                                                    
      *                                                                         
      *                                                                         
      ***************************************************************           
      * TP90 - I/O HANDLER.   OPEN / CLOSE FILES                                
      * FUNCTION CODES:                    RETURN CODES:                        
      *        OP  - OPEN                        0   - SUCCESSFUL               
      *        CL  - CLOSE                       1   - NOT FOUND                
      *        IN  - FILE INFORMATION            2   - END-OF-FILE              
      *        SB  - START BROWSE                B   - BAD PARAMETER            
      *        LO  - LOCATE                      E   - I/O ERROR                
      *        RD  - READ RECORD                 L   - LOGIC ERROR              
      *        BR  - READ NEXT                                                  
      *        WR  - WRITE RECORD                                               
      *        UP  - UPDATE RECORD                                              
      *        DL  - DELETE RECORD                                              
      ***************************************************************           
       0900-LB949-INPT-FILE.                                                    
      *                                                                         
           MOVE GVBTP90-VALUE-SEQUENTIAL  TO GVBTP90-FILE-TYPE                  
           MOVE GVBTP90-VALUE-INPUT       TO GVBTP90-FILE-MODE                  
                                                                                
           CALL WS-GVBTP90  USING GVBTP90-PARAMETER-AREA-INPT                   
                                  GVBTP90-RECORD-AREA                           
                                  GVBTP90-RECORD-KEY                            
           END-CALL                                                             
      *                                                                         
           IF  GVBTP90-RETURN-CODE NOT = GVBTP90-VALUE-SUCCESSFUL               
           AND GVBTP90-RETURN-CODE NOT = GVBTP90-VALUE-END-OF-FILE              
                PERFORM 9998-TP90-ERR                                           
           END-IF                                                               
           .                                                                    
      *                                                                         
      *                                                                         
      ***************************************************************           
      *  PROGRAM FINALIZATIONS:                                                 
      ***************************************************************           
       9900-FINALIZATION.                                                       
      *                                                                         
           DISPLAY 'GVBXR6: ' ' '                                               
           DISPLAY 'GVBXR6: '                                                   
               X95PARM2-EVENT-DDNAME ' '                                        
                   ' *** ALL INPUT FILES HAVE BEEN PROCESSED ***'               
           DISPLAY 'GVBXR6: ' ' '                                               
           .                                                                    
      *                                                                         
      *                                                                         
      ***************************************************************           
      *  DISPLAY MESSAGE OF # RECS READ FOR THIS FILE                           
      ***************************************************************           
       9910-NON-EMPTY-FILE.                                                     
      *                                                                         
           MOVE WS-FNCL-RCRDS-READ TO WS-DISPLAY-MASK-1                         
      *                                                                         
           DISPLAY 'GVBXR6: '                                                   
           DISPLAY 'GVBXR6: '                                                   
             WS-DISPLAY-MASK-1 ' = INPUT RCRDS READ FROM THIS PARTITION'        
           DISPLAY 'GVBXR6: '                                                   
      *                                                                         
           COMPUTE LS-RECORDS-READ =                                            
             LS-RECORDS-READ + WS-FNCL-RCRDS-READ                               
           COMPUTE LS-PARTITIONS-PROCESSED =                                    
             LS-PARTITIONS-PROCESSED + 1                                        
      *                                                                         
           IF LS-PARTITIONS-PROCESSED >= LS-PARTITIONS-TOTAL                    
              MOVE LS-RECORDS-READ TO WS-DISPLAY-MASK-1                         
              DISPLAY 'GVBXR6: '                                                
                WS-DISPLAY-MASK-1 ' = TOTAL RCRDS READ FROM '                   
                LS-PARTITIONS-PROCESSED ' PARTITIONS'                           
              DISPLAY 'GVBXR6: '                                                
           END-IF                                                               
           .                                                                    
      *                                                                         
      *                                                                         
      ***************************************************************           
      *  DISPLAY MESSAGE THAT FILE WAS EMPTY                                    
      ***************************************************************           
       9920-EMPTY-FILE.                                                         
      *                                                                         
      *                                                                         
           DISPLAY 'GVBXR6: '                                                   
           DISPLAY 'GVBXR6: '                                                   
                   'EMPTY INPUT FILE: '                                         
                   'ZERO INPUT RCRDS READ FROM THIS PARTITION ---'              
           DISPLAY 'GVBXR6: '                                                   
      *                                                                         
           COMPUTE LS-RECORDS-READ =                                            
             LS-RECORDS-READ + WS-FNCL-RCRDS-READ                               
           COMPUTE LS-PARTITIONS-PROCESSED =                                    
             LS-PARTITIONS-PROCESSED + 1                                        
      *                                                                         
           IF LS-PARTITIONS-PROCESSED >= LS-PARTITIONS-TOTAL                    
              MOVE LS-RECORDS-READ TO WS-DISPLAY-MASK-1                         
              DISPLAY 'GVBXR6: '                                                
                WS-DISPLAY-MASK-1 ' = TOTAL RCRDS READ FROM '                   
                LS-PARTITIONS-PROCESSED ' PARTITIONS'                           
              DISPLAY 'GVBXR6: '                                                
           END-IF                                                               
           .                                                                    
      *                                                                         
      *                                                                         
      ***************************************************************           
      *  DISPLAY ERROR MESSAGE FOR GVBTP90 CALLS AND ABEND.                     
      ***************************************************************           
       9998-TP90-ERR.                                                           
      *                                                                         
           DISPLAY 'GVBXR6: ' ' '                                               
           DISPLAY 'GVBXR6: '                                                   
               X95PARM2-EVENT-DDNAME '  ' GVBTP90-DDNAME                        
           DISPLAY 'GVBXR6: '                                                   
               '  FUNCTION = ' GVBTP90-FUNCTION-CODE                            
           DISPLAY 'GVBXR6: '                                                   
               '    REASON = ' GVBTP90-RETURN-CODE                              
           DISPLAY 'GVBXR6: '                                                   
               '    TYPE   = ' GVBTP90-FILE-TYPE                                
           DISPLAY 'GVBXR6: '                                                   
               '    MODE   = ' GVBTP90-FILE-MODE                                
           DISPLAY 'GVBXR6: '                                                   
               '   MODNAME = ' MODNAME                                          
           DISPLAY 'GVBXR6: '                                                   
               '      #READ= ' WS-FNCL-RCRDS-READ                               
           STOP RUN                                                             
           .                                                                    
