      *****************************************************************
      *                                                               *
      * (C) COPYRIGHT IBM CORPORATION 2008, 2022.                     *
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
      *   THE VALUE OF X95PARM1-ENV-DATA IS PASSED FROM GVBMR95.                
      *                                                                         
      *   DURING THE OPEN PHASE, READ EXITS SHOULD RETURN THE FIRST             
      *   EVENT RECORD.                                                         
      *                                                                         
       01  X95PARM1-ENV-DATA.                                                   
           05  X95PARM1-THREAD-NBR         PIC S9(04)  COMP.                    
           05  X95PARM1-PHASE-CODE         PIC  X(02).                          
               88  X95PARM1-OPEN-PHASE     VALUE 'OP'.                          
               88  X95PARM1-READ-PHASE     VALUE 'RD'.                          
               88  X95PARM1-CLOSE-PHASE    VALUE 'CL'.                          
           05  X95PARM1-CURRENT-VIEW-ID    PIC S9(09)  COMP.                    
           05  X95PARM1-ENV-VAR-TABLE-PTR  POINTER.                             
           05  X95PARM1-JOIN-STEP-COUNT    PIC S9(09)  COMP.                    
           05  X95PARM1-JOIN-STACK-PTR     POINTER.                             
           05  X95PARM1-PROCESS-DATE-TIME.                                      
               10  X95PARM1-PROCESS-DATE   PIC  X(08).                          
               10  X95PARM1-PROCESS-TIME   PIC  X(08).                          
           05  X95PARM1-ERROR-REASON       PIC S9(08)  COMP.                    
           05  X95PARM1-ERROR-BUFFER-PTR   POINTER.                             
           05  X95PARM1-ERROR-BUFFER-LEN   PIC S9(08)  COMP.                    
           05  X95PARM1-PARTITION-COUNT    PIC S9(08)  COMP.                    
           05  X95PARM1-THREAD-WORKAREA    POINTER.                             
                                                                                
       01  X95PARM2-EVENT-FILE-DATA.                                            
                                                                                
      *   THE VALUE OF X95PARM2-EVENT-DDNAME IS PASSED FROM GVBMR95.            
      *                                                                         
           05  X95PARM2-EVENT-DDNAME       PIC  X(08).                          
                                                                                
      *   THE VALUE OF X95PARM2-EVENT-REC-NBR IS PASSED FROM GVBMR95.           
      *   THIS IS NOW AN 8 BYTE BINARU COUNT                                    
           05  X95PARM2-EVENT-REC-NBR      PIC  9(16)  COMP.                    
                                                                                
      *   READ EXITS SET THE VALUE OF X95PARM2-EVENT-REC-FMT IN THE             
      *   OPEN PHASE AND PASS IT TO GVBMR95.  GVBMR95 PASSES THE                
      *   VALUE TO LOOKUP EXITS AND WRITE EXITS.                                
      *                                                                         
           05  X95PARM2-EVENT-REC-FMT      PIC  X(01).                          
               88  X95PARM2-FIXED-LENGTH       VALUE 'F'.                       
               88  X95PARM2-VARIABLE-LENGTH    VALUE 'V'.                       
               88  X95PARM2-DELIMITED          VALUE 'D'.                       
                                                                                
      *   READ EXITS SET THE VALUE OF X95PARM2-REC-FLD-DELIM IN THE             
      *   OPEN PHASE AND PASS IT TO GVBMR95.  GVBMR95 PASSES THE                
      *   VALUE TO LOOKUP EXITS AND WRITE EXITS.                                
      *                                                                         
           05  X95PARM2-REC-FLD-DELIM      PIC  X(01).                          
                                                                                
      *   READ EXITS SET THE VALUE OF X95PARM2-EVENT-REC-LEN ON EVERY           
      *   EXECUTION AND PASS IT TO GVBMR95.  GVBMR95 PASSES THE                 
      *   VALUE TO LOOKUP EXITS AND WRITE EXITS.                                
      *                                                                         
           05  X95PARM2-EVENT-REC-LEN      PIC S9(09)  COMP.                    
                                                                                
      *   READ EXITS SET THE VALUE OF X95PARM2-MAX-REC-LEN IN THE               
      *   OPEN PHASE AND PASS IT TO GVBMR95.  GVBMR95 PASSES THE                
      *   VALUE TO LOOKUP EXITS AND WRITE EXITS.                                
      *                                                                         
           05  X95PARM2-MAX-REC-LEN        PIC S9(09)  COMP.                    
                                                                                
      *   READ EXITS SET THE VALUE OF X95PARM2-MAX-BLOCK-SIZE IN THE            
      *   OPEN PHASE AND PASS IT TO GVBMR95.  GVBMR95 PASSES THE                
      *   VALUE TO LOOKUP EXITS AND WRITE EXITS.                                
      *                                                                         
           05  X95PARM2-MAX-BLOCK-SIZE     PIC S9(09)  COMP.                    
                                                                                
      *   THE LAYOUT OF X95PARM3-STARTUP-DATA IS USER-DEFINED.  FOR             
      *   READ EXITS, THE VALUE IS SET IN THE PHYSICAL FILE ENTITY.             
      *   FOR LOOKUP EXITS, THE VALUE IS SET IN LOGICAL RECORD ENTITY.          
      *   FOR WRITE EXITS, THE VALUE IS SET IN THE VIEW PROPERTIES              
      *   OR ON THE WRITE VERB IN THE VIEW LOGIC TEXT.                          
      *   THIS VALUE IS PASSED TO THE EXIT PROGRAM FROM GVBMR95.                
      *                                                                         
      *   TO POINT X95PARM3-STARTUP-DATA TO YOUR OWN STRUCTURE                  
      *   (FOR EXAMPLE, LS-STARTUP-PARMS), EXECUTE THE STATEMENT:               
      *                                                                         
      *    SET ADDRESS OF LS-STARTUP-PARMS TO                                   
      *        ADDRESS OF X95PARM3-STARTUP-DATA                                 
      *                                                                         
      *   THE FIRST TIME THIS PROGRAM IS GIVEN CONTROL.                         
      *                                                                         
       01  X95PARM3-STARTUP-DATA           PIC  X(32).                          
                                                                                
      *   X95PARM4-EVENT-REC-PTR IS NOT APPLICABLE TO READ EXITS.               
      *   FOR LOOKUP AND WRITE EXITS, THIS PARAMETER POINTS TO THE              
      *   ADDRESS OF THE CURRENT RECORD FROM THE EVENT FILE.                    
      *                                                                         
      *   TO POINT X95PARM4-EVENT-REC-PTR TO YOUR OWN STRUCTURE                 
      *   (FOR EXAMPLE, LS-EVENT-REC), EXECUTE THE STATEMENT:                   
      *                                                                         
      *    SET ADDRESS OF LS-EVENT-REC TO                                       
      *        X95PARM4-EVENT-REC-PTR-LOW                                       
      *                                                                         
      *   WHENEVER THIS PROGRAM IS GIVEN CONTROL.                               
      *                                                                         
       01  X95PARM4-EVENT-REC-PTR.                                              
           05  X95PARM4-EVENT-REC-PTR-HIGH     POINTER.                         
           05  X95PARM4-EVENT-REC-PTR-LOW      POINTER.                         
                                                                                
      *   X95PARM5-EXTRACT-REC IS NOT APPLICABLE TO READ EXITS.                 
      *   FOR LOOKUP AND WRITE EXITS, THIS PARAMETER POINTS TO THE              
      *   THE CURRENT EXTRACT RECORD.  THE LAYOUT OF THE BEGINNING              
      *   OF EACH EXTRACT RECORD IS FIXED, BUT THE LAYOUT OF THE REST           
      *   OF THE RECORD IS DETERMINED BY THE VIEW.                              
      *                                                                         
      *   TO POINT X95PARM5-VAR-LEN-AREA TO YOUR OWN STRUCTURE                  
      *   (FOR EXAMPLE, LS-EXTRACT-DATA), EXECUTE THE STATEMENT:                
      *                                                                         
      *    SET ADDRESS OF LS-EXTRACT-REC TO                                     
      *        ADDRESS OF X95PARM5-EXTRACT-VAR-LEN-AREA                         
      *                                                                         
      *   WHENEVER THIS PROGRAM IS GIVEN CONTROL.                               
      *                                                                         
       01  X95PARM5-EXTRACT-REC.                                                
           05  X95PARM5-EXTRACT-RDW.                                            
               10  X95PARM5-EXT-REC-LENGTH     PIC S9(04)  COMP.                
               10  FILLER                      PIC S9(04)  COMP.                
           05  X95PARM5-EXTRACT-PREFIX.                                         
               10  X95PARM5-SORT-KEY-LENGTH    PIC S9(04)  COMP.                
               10  X95PARM5-TITLE-KEY-LENGTH   PIC S9(04)  COMP.                
               10  X95PARM5-DATA-AREA-LENGTH   PIC S9(04)  COMP.                
               10  X95PARM5-NBR-CT-COLS        PIC S9(04)  COMP.                
               10  X95PARM5-VIEW-ID            PIC S9(09)  COMP.                
           05  X95PARM5-EXTRACT-VAR-LEN-AREA   PIC  X(01).                      
                                                                                
      *   THE LAYOUT OF X95PARM6-LOOKUP-KEY IS USER-DEFINED. FOR                
      *   READ AND WRITE EXITS, THIS PARAMETER IS NOT APPLICABLE.               
      *   FOR LOOKUP EXITS, THE VALUE IS SET IN THE VIEW LOGIC TEXT             
      *   OR IN JOIN STEP PARAMETERS.                                           
      *   THIS VALUE IS PASSED TO THE EXIT PROGRAM FROM GVBMR95.                
      *                                                                         
      *   TO POINT X95PARM6-LOOKUP-KEY TO YOUR OWN STRUCTURE                    
      *   (FOR EXAMPLE, LS-LOOKUP-PARMS), EXECUTE THE STATEMENT:                
      *                                                                         
      *    SET ADDRESS OF LS-LOOKUP-PARMS TO                                    
      *        ADDRESS OF X95PARM6-LOOKUP-KEY                                   
      *                                                                         
      *   WHENEVER THIS PROGRAM IS GIVEN CONTROL.                               
      *                                                                         
       01  X95PARM6-LOOKUP-KEY             PIC  X(256).                         
                                                                                
      *   GVBMR95 DOES NOT GUARANTEE THAT WORKING STORAGE IN A COBOL            
      *   PROGRAM WILL BE REMAIN IN THE SAME STATE ON SUBSEQUENT CALLS          
      *   TO THE PROGRAM, SO ANY DATA ITEMS WHICH NEED TO BE RETAINED           
      *   BETWEEN CALLS SHOULD BE PLACED IN A DYNAMICALLY ALLOCATED             
      *   AREA.                                                                 
      *                                                                         
      *   DURING THE OPEN PHASE, THE GENEVA ROUTINE GVBUR05 SHOULD BE           
      *   CALLED TO ALLOCATE A WORK AREA AND X95PARM7-WORK-AREA-ANCHOR          
      *   SHOULD BE SET TO POINT TO IT.                                         
      *                                                                         
      *   FOR EXAMPLE:                                                          
      *                                                                         
      *    MOVE LENGTH OF LS-WORK-AREA TO WS-WORK-AREA-LENGTH                   
      *                                                                         
      *    CALL GVBUR05                                                         
      *        USING X95PARM7-WORK-AREA-ANCHOR                                  
      *              WS-WORK-AREA-LENGTH                                        
      *                                                                         
      *    SET ADDRESS OF LS-WORK-AREA TO                                       
      *        X95PARM7-WORK-AREA-ANCHOR                                        
      *                                                                         
      *   ON SUBSEQUENT CALLS, X95PARM7-WORK-AREA-ANCHOR RETAINS ITS            
      *   VALUE AND MAY BE POINTED TO THE STRUCTURE AGAIN.                      
      *   FOR EXAMPLE:                                                          
      *                                                                         
      *    SET ADDRESS OF LS-WORK-AREA TO                                       
      *        X95PARM7-WORK-AREA-ANCHOR                                        
      *                                                                         
       01  X95PARM7-WORK-AREA-ANCHOR       POINTER.                             
                                                                                
      *   X95PARM8-RETURN-CODE SHOULD BE SET IN THE EXIT PROGRAM                
      *   TO THE NUMERIC CODE TO BE RETURNED TO GVBMR95.                        
      *                                                                         
       01  X95PARM8-RETURN-CODE            PIC S9(09)  COMP.                    
      *  ALL EXITS:                                                             
           88  X95PARM8-SUCCESSFUL             VALUE +0.                        
      *  LOOKUP EXITS ONLY:                                                     
           88  X95PARM8-NOT-FOUND              VALUE +4.                        
      *  WRITE EXITS ONLY:                                                      
           88  X95PARM8-WRITE-AND-REPEAT-CALL  VALUE +4.                        
      *  READ EXITS ONLY:                                                       
           88  X95PARM8-END-OF-FILE            VALUE +8.                        
      *  LOOKUP EXITS ONLY:                                                     
           88  X95PARM8-SKIP-EVENT-REC         VALUE +8.                        
      *  WRITE EXITS ONLY:                                                      
           88  X95PARM8-SKIP-EXTRACT-REC       VALUE +8.                        
      *  ALL EXITS:                                                             
           88  X95PARM8-DISABLE-CURRENT-VIEW   VALUE +12.                       
           88  X95PARM8-ABORT-RUN              VALUE +16.                       
                                                                                
      *   X95PARM9-RESULT-PTR SHOULD BE SET IN THE EXIT PROGRAM                 
      *   TO THE ADDRESS OF THE STRUCTURE TO BE RETURNED TO GVBMR95.            
      *                                                                         
      *   TO POINT X95PARM9-RESULT-PTR TO YOUR OWN STRUCTURE                    
      *   (FOR EXAMPLE, WS-RESULT), EXECUTE THE STATEMENT:                      
      *                                                                         
      *    SET X95PARM9-RESULT-PTR TO                                           
      *        ADDRESS OF WS-RESULT                                             
      *                                                                         
      *   WHENEVER THIS PROGRAM IS GIVEN CONTROL.                               
      *                                                                         
       01  X95PARM9-RESULT-PTR.                                                 
           05  X95PARM9-RESULT-PTR-HIGH        POINTER.                         
           05  X95PARM9-RESULT-PTR-NUMERICH    REDEFINES                        
               X95PARM9-RESULT-PTR-HIGH        PIC S9(09)  COMP.                
           05  X95PARM9-RESULT-PTR-LOW         POINTER.                         
           05  X95PARM9-RESULT-PTR-NUMERICL    REDEFINES                        
               X95PARM9-RESULT-PTR-LOW         PIC S9(09)  COMP.                
                                                                                
      *  LOOKUP EXITS ONLY:                                                     
      *   THOUGH OPTIONAL, IT IS A GOOD EXIT PROGRAMMING PRACTICE TO            
      *   MOVE -1 TO X95PARM9-RESULT-PTR-NUMERIC WHEN THE LOOKUP RESULT         
      *   IS "NOT FOUND".  THIS GUARANTEES THAT THE POINTER IS NO               
      *   LONGER POINTING TO VALID DATA.                                        
      *                                                                         
       01  X95PARM9-RESULT-PTR-NUMERIC     REDEFINES                            
           X95PARM9-RESULT-PTR             PIC S9(09)  COMP.                    
                                                                                
      *  READ EXITS ONLY:                                                       
      *   READ EXITS SHOULD SET THE VALUE OF X95PARMA-RESULT-BLOCK-SIZE         
      *   TO THE LENGTH OF THE BLOCK POINTED TO BY                              
      *   X95PARM9-RESULT-PTR.                                                  
      *                                                                         
       01  X95PARMA-RESULT-BLOCK-SIZE      PIC S9(09)  COMP.                    
