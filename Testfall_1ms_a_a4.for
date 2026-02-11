C     *****************************************************************
C     *****************************************************************
C     *** Routine zur Durchführung von WPS-Simulationen in Abaqus   ***
C     *** MPA Universitaet Stuttgart ***
C     *** Abteilung Berechnung, Referat Schweiß- und Fügetechnik ***
C     *****************************************************************
C     *****************************************************************
C     *** Date: 2020/11/24 ***
C     *****************************************************************
C     *****************************************************************


        program Aufruf
        
        
C     *****************************************************************
C     *****************************************************************
C     *** Variablendefinitionen ***     
C     *****************************************************************  

        character (len=256) AufrufMechanischeRechnung
        character (len=40) RestartMechFileName
        character (len=40) RestartMechFileNameOld
        character (len=40) RestartMechFileNameFull
        character (len=30) MechStepNameFull
        character (len=100) RestartMechName
        character (len=100) MechName
        character (len=100) MechNameFULL
        character (len=100) ELEKName
        character (len=100) ELEKNameFULL
        character (len=100) UVARMNAME
        character (len=100) UVARMNAMEFULL
        character (len=10) UVARM
        character (len=11) Mech
        character (len=11) ELEK
        character (len=4)  StringEndungInp
        character (len=4)  StringEndungtxt
        character (len=40) StringRestartMechNumber
        character (len=20) MechStepName
        character(len=40)  StringMechStepNameNumber
        Character(len=30)  Datum
        character (len=100) ELEKINC
        character (len=100) ELEKINCNAME
        character (len=100) ELEKINCNAMEFULL
        character (len=4) STRINGENDUNGOUT
C****************************************************
        character (len=4)  StringEndungFil
        character (len=4) STRINGENDUNGCOM
        character (len=4) STRINGENDUNGMSG
        character (len=4) STRINGENDUNGPRT
        character (len=4) STRINGENDUNGSTA
        character (len=4) STRINGENDUNGdat
        character (len=4) STRINGENDUNGodb
        character (len=4) STRINGENDUNGexe
        character (len=4) StringEndungobj
        character*256 DEL
        character (len=40) DELNAME

c****************************************************
        integer FPERTNUMBER
        integer MECHINK
        character (len=40) FpertName
        character (len=40) FpertFileName
        character (len=40) FpertFileNameFull
        character (len=4) StringEndungFor
        character (len=256) AufrufFpert, AufrufFPERT2
C****************************************************
        integer Eleknumber
        character (len=100) RestartElekName
        character (len=40) RestartElekFileName
        character (len=40) RestartElekFileNameOld
        character (len=40) RestartElekFileNameFull
        character (len=256) AufrufElektrischeRechnung

c        integer Eleknumber
        character (len=100) RestartElekNameFIELD
        character (len=40) RestartElekFileNameFIELD
        character (len=40) RestartElekFileNameOldFIELD
        character (len=40) RestartElekFileNameFullFIELD
c        character (len=256) AufrufElektrischeRechnung

C********************************************************
C       19.08.2010 Variablen zur Iteration
        character (len=100) EPOTNAME
        character (len=4) STRINGENDUNGEPOT
        double precision EPOTVALUE(0:50,1:4)
        double precision EPOTVALUEEXPERIMENT(0:1000,1:4)
        double precision EPOTCOORDAKTUELL(1:3)
        double precision EPOTVALUEAKTUELL
        integer EPOTNODEAKTUELL
        double precision EPOTDIFFERENZ(0:50,1:3)
        character (len=100) IterationElekName
        character (len=100) IterationElekNameZus
        character (len=100) IterationElekNameLW
        character (len=100) IterationElekNameFIELD
        character (len=100) IterationElekFileNameFIELD
        character (len=100)  IterationElekFileName
        character (len=100)  IterationElekFileNameZus
        character (len=100)  IterationElekFileNameLW
        character (len=40) StringIterationElekNumber
        character (len=100) IterationElekFileNameFull
        character (len=100) IterationElekFileNameZusFull
        character (len=100) IterationElekFileNameLWFull
        character (len=100) IterationElekFileNameFullFIELD
        Integer IntegerIterationElekNumber
        integer ITNUMBER,k
        double precision EPOTDIFFERENZ_ELBL1(1:50)
        double precision EPOTDIFFERENZ_BLBL(1:50)
        double precision EPOTDIFFERENZ_ELBL2(1:50)
        double precision EPOTDIFFERENZ_ELBL1_VORGABE
        double precision EPOTDIFFERENZ_BLBL_VORGABE
        double precision EPOTDIFFERENZ_ELBL2_VORGABE
        double precision QUALITAET_ELBL1(1:50)
        double precision QUALITAET_BLBL(1:50)
        double precision QUALITAET_ELBL2(1:50)
        double precision SUMME_QUALITAET(1:50)
        double precision Gesamtstrom
        character (len=100) FILE_Kontaktflaeche
        integer kontakt, kk
        double precision Kontaktflaeche(1:6)
        double precision LEITWERT_ELBL1(0:50)
        Double precision LEITWERT_ELBL1_SPEZ(0:50)
        double precision LEITWERT_BLBL(0:50)
        Double precision LEITWERT_BLBL_SPEZ(0:50)
        double precision LEITWERT_ELBL2(0:50)
        Double precision LEITWERT_ELBL2_SPEZ(0:50)
        integer FPERTNUMBIT
        integer ANZAHLITERATIONEN
C********************************************************
        double precision DR1, DR2, DR3
        double precision Widerstand1, Widerstand2, Widerstand3
        double precision Kontaktwiderstand1, Kontaktwiderstand2
        double precision Kontaktwiderstand3

C********************************************************


        character*40 a,b,c
        integer IntegerRestartMechNumber
        integer IntegerRestartStepNumber
        integer FILENUMBER
        integer STEP, Inkrement
        integer LAUF, dummy
        double precision JouleFraction
        character*40 DataCurrent, DataForce, DataContact
        integer NumberDataSet
        character*16 checkListEnd
        double precision Strom(:)
        double precision Kraft(:)
        allocatable :: Strom
        allocatable :: Kraft
        double precision CurrentArea
        double precision VORZEICHEN
        double precision Kontakt_EL_EL(1:1000), Kontakt_EL_BL(1:1000)
        integer ELEK1INK
        integer Knoten
        integer Elemente
        integer Spannungsabgriff1, Spannungsabgriff2
        integer Spannungsabgriff3, Spannungsabgriff4         
        
        integer ios, ios2
        logical inNode, inElem
        character*512 line
        INTEGER collecting, pos, p2, i, L, nid
        LOGICAL got1, got2, got3, got4
        CHARACTER*512 uline, name, tmp
        
C     *****************************************************************
C     *** Definition der Knoten- und Elementanzahl ****
C     *****************************************************************
C        Knoten=25709
C        Elemente=25170
        Knoten=0
        Elemente=0
        ios=0
        inNode=.FALSE.
        inElem=.FALSE.
        
        open(901,file='WPS_MECH_Netz_Assembly.txt',status='old',
     &  action='read',iostat=ios,form='formatted')
        if(ios .NE. 0) THEN
          print*, '*** Warning: Mesh-Datei nicht gefunden'
        else
10      continue
          read(901, '(A)', iostat=ios) line
          if (ios .NE. 0) goto 20
          if (line(1:1) .EQ. '*') THEN
              inNode = (line(1:5) .EQ. '*Node' .OR. line(1:5) 
     &                  .EQ. '*NODE')
              inElem = (line(1:8) .EQ. '*Element' .OR. line(1:8) 
     &                  .EQ. '*ELEMENT')
          else
              if (inNode) THEN
                  if (len_trim(line) .GT. 0) Knoten = Knoten + 1
              else if (inElem) THEN
                  if (len_trim(line) .GT. 0) Elemente = Elemente + 1
              end if
          end if
          goto 10
20        continue
          close(901)
          print*, 'Knoten= ', Knoten, ' Elemente= ', Elemente
        end if
        
        Spannungsabgriff1=-1    	!Abgriff Elektrode unten
        Spannungsabgriff2=-1     !Abgriff Blech unten             
        Spannungsabgriff3=-1	  	!Abgriff Blech oben
        Spannungsabgriff4=-1      !Abgriff Elektrode oben

C --- locals for Nset parsing ------------------------------------------

        collecting = 0
        got1 = .FALSE.
        got2 = .FALSE.
        got3 = .FALSE.
        got4 = .FALSE.


        OPEN(901,FILE='WPS_MECH_Netz_Assembly.txt',STATUS='OLD',
     &     ACTION='READ',IOSTAT=ios,FORM='FORMATTED')
        IF (ios .NE. 0) THEN
            PRINT*, '*** Warning: Mesh-Datei nicht gefunden (Nset)'
        ELSE
110         CONTINUE
            READ(901,'(A)',IOSTAT=ios) line
            IF (ios .NE. 0) GOTO 120


            uline = line
            DO i = 1, LEN(uline)
            IF (uline(i:i) .GE. 'a' .AND. uline(i:i) .LE. 'z') THEN
                uline(i:i) = CHAR(ICHAR(uline(i:i)) - 32)
            ENDIF
            ENDDO

            IF (uline(1:1) .EQ. '*') THEN
            collecting = 0
            IF (INDEX(uline,'*NSET') .GT. 0) THEN
                pos = INDEX(uline,'NSET=')
                IF (pos .GT. 0) THEN
                    name = uline(pos+5:)
                    p2 = INDEX(name,',')
                    IF (p2 .GT. 0) name = name(1:p2-1)


                    tmp = ' '
                    L = LEN_TRIM(name)
                    DO i = 1, L
                        IF (name(i:i) .NE. ' ') THEN
                        tmp(LEN_TRIM(tmp)+1:LEN_TRIM(tmp)+1)=name(i:i)
                        ENDIF
                    ENDDO
                    name = tmp(1:LEN_TRIM(tmp))


                    IF ( (name .EQ. 'SP_UNTER_ELEKTRODE') .AND.
     &                 (.NOT. got1) ) THEN
                        collecting = 1
                    ELSE IF ( (name .EQ. 'SP_BLECH2') .AND.
     &                      (.NOT. got2) ) THEN
                        collecting = 2
                    ELSE IF ( (name .EQ. 'SP_BLECH1') .AND.
     &                      (.NOT. got3) ) THEN
                        collecting = 3
                    ELSE IF ( (name .EQ. 'SP_OBER_ELEKTRODE') .AND.
     &                      (.NOT. got4) ) THEN
                        collecting = 4
                    ELSE
                        collecting = 0
                    ENDIF
                ENDIF
            ENDIF

            ELSE

            IF (collecting .GT. 0) THEN
                tmp = line
                DO i = 1, LEN_TRIM(tmp)
                    IF (tmp(i:i) .EQ. ',') tmp(i:i) = ' '
                ENDDO
                nid = -1
                READ(tmp,*,IOSTAT=ios2) nid
                IF (ios2 .EQ. 0 .AND. nid .GT. 0) THEN
                    IF (collecting .EQ. 1) THEN
                        Spannungsabgriff1 = nid
                        got1 = .TRUE.
                    ELSE IF (collecting .EQ. 2) THEN
                        Spannungsabgriff2 = nid
                        got2 = .TRUE.
                    ELSE IF (collecting .EQ. 3) THEN
                        Spannungsabgriff3 = nid
                        got3 = .TRUE.
                    ELSE IF (collecting .EQ. 4) THEN
                        Spannungsabgriff4 = nid
                        got4 = .TRUE.
                    ENDIF
                    collecting = 0
                ENDIF
            ENDIF
            ENDIF

            GOTO 110
120         CONTINUE
            CLOSE(901)
        ENDIF


        PRINT*, '[Nset->SA] 1..4 =', Spannungsabgriff1,
     &         Spannungsabgriff2, Spannungsabgriff3,
     &         Spannungsabgriff4

        IF (Spannungsabgriff1 .LT. 0) PRINT*,
     &  '  Warnung: SP_UNTER_ELEKTRODE nicht gefunden'
        IF (Spannungsabgriff2 .LT. 0) PRINT*,
     &  '  Warnung: SP_BLECH2 nicht gefunden'
        IF (Spannungsabgriff3 .LT. 0) PRINT*,
     &  '  Warnung: SP_BLECH1 nicht gefunden'
        IF (Spannungsabgriff4 .LT. 0) PRINT*,
     &  '  Warnung: SP_OBER_ELEKTRODE nicht gefunden'    
      
C     *****************************************************************
C     *** Spannungsabgriffe des numerischen Modells**
C     *** Hier müssen Knoten definiert werden ****
C     *****************************************************************
C        Spannungsabgriff1=19838    	!Abgriff Elektrode unten
C        Spannungsabgriff2=13362     !Abgriff Blech unten             
C        Spannungsabgriff3=46	  	!Abgriff Blech oben
C        Spannungsabgriff4=6418      !Abgriff Elektrode oben
        
C        A
C        Spannungsabgriff1=22508    	!Abgriff Elektrode unten
C        Spannungsabgriff2=11861     !Abgriff Blech unten             
C        Spannungsabgriff3=46	  	!Abgriff Blech oben
C        Spannungsabgriff4=11072      !Abgriff Elektrode oben

C       B        
C        Spannungsabgriff1=15629    	!Abgriff Elektrode unten
C        Spannungsabgriff2=13352     !Abgriff Blech unten             
C        Spannungsabgriff3=426	  	!Abgriff Blech oben
C        Spannungsabgriff4=2777      !Abgriff Elektrode oben
C     *****************************************************************                                
        
C**********Einlesen des Kraft- und Stromverlaufs******************
!      definiere I/O-Files
C       print*, 'Strom - Eingabedatei = ? (Stromverlauf.txt)'
C       read(5,'(A)') DataCurrent
C       print*, 'Kraft - Eingabedatei = ? (Kraftverlauf.txt)'
C       read(5,'(A)') DataForce
C       print*, 'Kontakt - Eingabedatei = ? (Potentialverlauf.txt)'
C       read(5,'(A)') DataContact       

       DataCurrent = ''
       DataForce = ''
       DataContact = ''

       if(DataCurrent.eq.'') then
               DataCurrent='Stromverlauf.txt'
       end if 
       if(DataForce.eq.'') then
               DataForce='Kraftverlauf.txt'
       end if 
       if(DataContact.eq.'') then
               DataContact='Potentialverlauf.txt'
       end if 

       print*, DataCurrent
       print*, DataForce
       print*, DataContact
       
      
      open(42,file=DataCurrent, status='UNKNOWN')
      open(43,file=DataForce, status='UNKNOWN')
      open(52,file=DataContact, status='UNKNOWN')
     
           
      numberDataSet=1000

       do i=1,numberDataSet
       read(42,*) checkListEnd
       if(checkListEnd.eq.'Ende') then
       goto 123
       end if      
       end do
C       PAUSE

C      Strom und Kraft - gleiche Datengröße

123    continue
       close(42)

       open(44,file=DataCurrent)

       print*, i-1, ' Datensaetze vorhanden'
       numberDataSet=i-2
c       PAUSE

       allocate(Strom(0:numberDataSet))
       allocate(Kraft(0:numberDataSet))
       
C       Berechnung der Elektrodenfläche, Strom wird flächenspezifisch
C       als SurfaceLoad aufgebracht - dementsprechend muss hier die
C       Fläche berechnet bzw. vorgegeben werden.
      
       CurrentArea=314.159 !Pi/4*20^2 für Schweißen von Aluminium (d=20mm)

       do i=0,numberDataSet-1
            read(44,*)Dummy,Strom(i)
            read(43,*)Dummy,Kraft(i)
       print*,i, Strom(i), Kraft(i)
       Strom(i)=(Strom(i)*1000)/CurrentArea
       Kraft(i)=-Kraft(i)
       end do
c       pause
       close(44)
       close(43)
C************nicht allgemein*********************
C      23.08.10 Ersatz spezifischer Kontaktleitwert mit lokalem
C      Potential an Spannungsabgriffsstellen aus Experiment. 

       
       do i=0,1000 !vorübergehend
       read(52,*) dummy, EPOTVALUEEXPERIMENT(i,1), 
     &EPOTVALUEEXPERIMENT(i,2), EPOTVALUEEXPERIMENT(i,3),
     &EPOTVALUEEXPERIMENT(i,4)

       print*, EPOTVALUEEXPERIMENT(i,1),EPOTVALUEEXPERIMENT(i,2),
     &EPOTVALUEEXPERIMENT(i,3),EPOTVALUEEXPERIMENT(i,4)
c       pause
       end do
c       PAUSE

C       Einlesen der spezifischen Kontaktleitwerte aus Vorgabedatei    
c       do i=26,400

C*****************************************************************       
c       Kontakt_EL_BL(25)=100000000.d0
c       Kontakt_EL_EL(25)=100000000.d0
c       Kontakt_EL_BL(i)=Kontakt_EL_BL(25) !Konstant
c       Kontakt_EL_EL(i)=Kontakt_EL_EL(25) !Konstant
c       end do
       
       
C*****************************************************************
**
**


        STRINGENDUNGCOM='.com'
        STRINGENDUNGMSG='.msg'
        STRINGENDUNGPRT='.prt'
        STRINGENDUNGSTA='.sta'
        STRINGENDUNGdat='.dat'
        STRINGENDUNGodb='.odb'
        STRINGENDUNGexe='.exe'
        StringEndungobj='.obj'
        STRINGENDUNGINP='.inp'
        STRINGENDUNGTXT='.txt'


        FILENUMBER=1
        
C     *****************************************************************
C     ***  Aus den Variablen Datum und Mech wird der Dateiname
C     zusammengesetzt ***        
C     *****************************************************************    
        Datum='241120'
        Mech='_MECH_START'

        MECHNAME=trim(Datum)//trim(MECH)
        MechnameFULL=trim(Datum)//trim(MECH)//trim(StringEndungInp)
        print*, MechnameFULL
        
C     *****************************************************************
C     ***  Erstellen der Abaqus-Input-Datei (mechanisch) ***        
C     *****************************************************************       
        open(FILENUMBER,file=trim(MechnameFULL), STATUS='UNKNOWN')
C     *****************************************************************
C     ***  Netz und Materialdefinition ***
C     *** Folgende Flächen müssen doppelt(!!!)definiert werden
c     *** Kontaktfläche Elektrode 1 - Blech
c     *** Kontaktfläche Blech - Blech
c     *** Kontaktfläche Elektrode 2 - Blech
C     *** Doppelte Definition ist notwendig zum Umschalten zwischen
C     *** den Kontaktdefinitionen "NOSEP" und "SEP"
C     *****************************************************************          
        write(FILENUMBER,*)'*INCLUDE, input=WPS_MECH_Netz_Assembly.txt'
        write(FILENUMBER,*)'*INCLUDE, input=WPS_MECH_MATERIAL
     &_MECHANISCH.txt'
C     *****************************************************************
C     *****************************************************************
C     *** Kontakteigenschaftsdefinition ***
C     *** Kontakteigenschaften müssen doppelt definiert werden,
C     *** da ein Öffnen des Kontakts bei abhebender Elektrode realisiert
C     *** werden muss ****        
C     *****************************************************************       
        write(FILENUMBER,*)'*Surface Interaction, name=ELEKTRODE'
        write(FILENUMBER,*)'1.,'
        write(FILENUMBER,*)'*Friction, slip tolerance=0.005'
        write(FILENUMBER,*)' 0.1 '
        write(FILENUMBER,*)'*Surface Behavior, 
     &pressure-overclosure=HARD'
        write(FILENUMBER,*)'*Surface Interaction, name=ELEK_NOSEP'
        write(FILENUMBER,*)'1.,'
        write(FILENUMBER,*)'*Friction, slip tolerance=0.005'
        write(FILENUMBER,*)' 0.1 '
        write(FILENUMBER,*)'*Surface Behavior, 
     &pressure-overclosure=HARD, no separation'
        write(FILENUMBER,*)'*Surface Interaction, name=BLECH'
        write(FILENUMBER,*)'1.'
        write(FILENUMBER,*)'*Friction, slip tolerance=0.005'
        write(FILENUMBER,*)' 0.1,'
        write(FILENUMBER,*)'*Surface Behavior, 
     &pressure-overclosure=HARD'
        write(FILENUMBER,*)'*Surface Interaction, name=BLECH_NOSEP'
        write(FILENUMBER,*)'1.'
        write(FILENUMBER,*)'*Friction, slip tolerance=0.005'
        write(FILENUMBER,*)' 0.1,'
        write(FILENUMBER,*)'*Surface Behavior, 
     &pressure-overclosure=HARD, no separation'
C     *****************************************************************
C     ***  Randbedingungen ***        
C     *****************************************************************  
        write(FILENUMBER,*)'*Initial Conditions, type=TEMPERATURE '
        write(FILENUMBER,*)'NALL, 20.'
c        write(FILENUMBER,*)'*Initial Conditions, type=FIELD '
c        write(FILENUMBER,*)'NALL, 1.'
        write(FILENUMBER,*)'*Boundary '
        write(FILENUMBER,*)'BC_Elektrode, ENCASTRE'
        write(FILENUMBER,*)'*Boundary '
        write(FILENUMBER,*)'BC_TEMP, ENCASTRE'
c     *****************************************************************
C     ***  Kontaktpaare (doppelt!!) ***        
C     *****************************************************************        

        write(FILENUMBER,*)'*Contact Pair, interaction=ELEKTRODE,
     &small sliding, type=SURFACE TO SURFACE'
        write(FILENUMBER,*)'SURF_BL1_EL1, SURF_EL1_BL1'
        
        write(FILENUMBER,*)'*Contact Pair, interaction=BLECH,
     &small sliding, type=SURFACE TO SURFACE'
        write(FILENUMBER,*)'SURF_BL2_BL1, SURF_BL1_BL2'

        write(FILENUMBER,*)'*Contact Pair, interaction=ELEKTRODE,
     &small sliding, type=SURFACE TO SURFACE'
        write(FILENUMBER,*)'SURF_BL2_EL2, SURF_EL2_BL2'

        write(FILENUMBER,*)'*Contact Pair, interaction=ELEK_NOSEP,
     &small sliding, type=SURFACE TO SURFACE'
        write(FILENUMBER,*)'SURF2_BL1_EL1, SURF2_EL1_BL1'

        write(FILENUMBER,*)'*Contact Pair, interaction=BLECH_NOSEP,
     &small sliding, type=SURFACE TO SURFACE'
        write(FILENUMBER,*)'SURF2_BL2_BL1, SURF2_BL1_BL2'

        write(FILENUMBER,*)'*Contact Pair, interaction=ELEK_NOSEP,
     &small sliding, type=SURFACE TO SURFACE'
        write(FILENUMBER,*)'SURF2_BL2_EL2, SURF2_EL2_BL2'
C         
C     *****************************************************************
C     ***  Schrittdefinition *** 
C     ***  Step1: Definiton des Kontakts       
C     *****************************************************************
C      
        write(FILENUMBER,*)'*Step, name=Step-1, nlgeom=yes'
        write(FILENUMBER,*)'*Static'
        write(FILENUMBER,*)'0.1, 1., 1e-05, 1.'
        write(FILENUMBER,*)'*MODEL CHANGE, type=Contact Pair, remove'
        write(FILENUMBER,*)'SURF2_BL2_BL1, SURF2_BL1_BL2'
        write(FILENUMBER,*)'SURF2_BL1_EL1, SURF2_EL1_BL1'
        write(FILENUMBER,*)'SURF2_BL2_EL2, SURF2_EL2_BL2'
        write(FILENUMBER,*)'*Boundary'
        write(FILENUMBER,*)'BC_Elektrode, ENCASTRE'
        write(FILENUMBER,*)'*Restart, write, frequency=0'
        write(FILENUMBER,*)'*Output, field, variable=PRESELECT'
c        write(FILENUMBER,*)'*Element Output, directions=YES'
c        write(FILENUMBER,*)'FV, SDV'
        write(FILENUMBER,*)'*NODE OUTPUT'
        write(FILENUMBER,*)'COORD'
        write(FILENUMBER,*)'*Output, history, variable=PRESELECT'
        write(FILENUMBER,*)'*CONTACT OUTPUT'
        write(FILENUMBER,*)'CAREA'
        write(FILENUMBER,*)'*End Step'
C     *****************************************************************
C     ***  Schrittdefinition *** 
C     ***  Step2: Verschiebungskontrollierte Kontaktaufnahme       
C     *****************************************************************
        write(FILENUMBER,*)'*Step, name=Step-2, nlgeom=yes'
        write(FILENUMBER,*)'*Static'
        write(FILENUMBER,*)'0.1, 1., 1e-05, 1.'
        write(FILENUMBER,*)'*CONTACT CONTROLS, STABILIZE'
		write(FILENUMBER,*)'*Boundary, OP=NEW'
        write(FILENUMBER,*)'BC_Elektrode, ENCASTRE'
        write(FILENUMBER,*)'*BOUNDARY, OP=NEW'
        write(FILENUMBER,*)'NSET_E1, 2,2, -0.001'
        write(FILENUMBER,*)'*Restart, write, frequency=0'
        write(FILENUMBER,*)'*Output, field, variable=PRESELECT'
c        write(FILENUMBER,*)'*Element Output, directions=YES'
c        write(FILENUMBER,*)'FV, SDV'
        write(FILENUMBER,*)'*NODE OUTPUT'
        write(FILENUMBER,*)'COORD'
        write(FILENUMBER,*)'*CONTACT OUTPUT, variable=ALL'
        write(FILENUMBER,*)'*Output, history, variable=PRESELECT'
        write(FILENUMBER,*)'*CONTACT OUTPUT'
        write(FILENUMBER,*)'CAREA'
        write(FILENUMBER,*)'*End Step'
C     *****************************************************************
C     ***  Schrittdefinition *** 
C     ***  Step3: Kraftaufbringung und Lösen der
C     ***  Verschiebungsrandbedingung ***      
C     *****************************************************************        
        write(FILENUMBER,*)'*Step, name=Step-3, nlgeom=yes'
        write(FILENUMBER,*)'*Static'
        write(FILENUMBER,*)'0.1, 1., 1e-05, 1.'
        write(FILENUMBER,*)'*Boundary, OP=NEW'
        write(FILENUMBER,*)'BC_Elektrode, ENCASTRE'
        write(FILENUMBER,*)'*Cload'
        write(FILENUMBER,*)'LOAD_ELEKTRODE_REFERENZ, 2,',Kraft(0)
        write(FILENUMBER,*)'*Restart, write, frequency=1,overlay'
C     *****************************************************************
C     *** Fil-File Definiton ***      
C     *****************************************************************        
        write(FILENUMBER,*)'*NODE FILE'
        write(FILENUMBER,*)'U'
        write(FILENUMBER,*)'*CONTACT FILE'
        write(FILENUMBER,*)'CAREA'
        write(FILENUMBER,*)'*CONTACT FILE, slave=SURF_BL2_BL1'
        write(FILENUMBER,*)'*CONTACT FILE, slave=SURF_BL1_EL1'
        write(FILENUMBER,*)'*CONTACT FILE, slave=SURF_BL2_EL2'
        write(FILENUMBER,*)'*CONTACT FILE, slave=SURF2_BL2_BL1'
        write(FILENUMBER,*)'*CONTACT FILE, slave=SURF2_BL1_EL1'
        write(FILENUMBER,*)'*CONTACT FILE, slave=SURF2_BL2_EL2'
C     *****************************************************************
C     *** ODB-Definitionen ***      
C     *****************************************************************        
        write(FILENUMBER,*)'*Output, field, variable=PRESELECT'
c        write(FILENUMBER,*)'*Element Output, directions=YES'
c        write(FILENUMBER,*)'FV, SDV'
        write(FILENUMBER,*)'*CONTACT OUTPUT, variable=ALL'
        write(FILENUMBER,*)'*NODE OUTPUT'
        write(FILENUMBER,*)'COORD'
        write(FILENUMBER,*)'*NODE PRINT, nset=NALL'
        write(FILENUMBER,*)'COORD'
        write(FILENUMBER,*)'*Output, history, variable=PRESELECT'
        write(FILENUMBER,*)'*CONTACT OUTPUT'
        write(FILENUMBER,*)'CAREA'
        write(FILENUMBER,*)'*End Step'
        close(FILENUMBER)
C     *****************************************************************
C
C     *****************************************************************
C     ***  Subroutine zur Definiton Out-File: Informationen zu Step- und
C     Inkrement
C     ***  Subroutine zur Definiton Fv1-File: Informationen zu
C     Temperaturwerten an den Integrationspunkten zur Einbindung in
C     Modelle zur Beschreibung der Phasenumwandlungskinetik
C     *****************************************************************  
        FILENUMBER=FILENUMBER+1
        UVARM='_WPS_UVARM'
        STRINGENDUNGFOR='.for'
        UVARMNAME=trim(Datum)//trim(UVARM)
        UVARMNAMEFULL=trim(Datum)//trim(UVARM)//trim(StringEndungFOR)
        print*, UVARMNAMEFULL

       open(FILENUMBER,file=trim(UVARMNAMEFULL), STATUS='UNKNOWN')
       write(FILENUMBER,*)'      module global'
       write(FILENUMBER,*)"      save"
       write(FILENUMBER,*)'      integer AktuellerStep'
       write(FILENUMBER,*)'      integer AktuellesInk '
       write(FILENUMBER,*)'      double precision ARRAY_FV                 ! Anzahl Elemente / Integrationspunkte
     &(1:',Elemente,',1:4)'
c       write(FILENUMBER,*)'      double precision TEMP2'
       write(FILENUMBER,*)'      end module global'
       write(FILENUMBER,*)'      SUBROUTINE UEXTERNALDB(LOP,LRESTART,
     &TIME,DTIME,KSTEP,KINC)'
       write(FILENUMBER,*)'      use global'
       write(FILENUMBER,*)"      include 'ABA_PARAM.INC'"
       write(FILENUMBER,*)'      CHARACTER*256 JOBNAME'
       write(FILENUMBER,*)'      CHARACTER xoutdir*255, xfname*80'
       write(FILENUMBER,*)'      CHARACTER dmkname*255, fout*255'
       write(FILENUMBER,*)'      CHARACTER fout2*255'
       write(FILENUMBER,*)'      INTEGER FLAG, I'
       write(FILENUMBER,*)"110   FORMAT(I6,4(',',1PE14.6))" 
       write(FILENUMBER,*)'      call GETJOBNAME( xfname, lxfname )'
       write(FILENUMBER,*)'      call GETOUTDIR( xoutdir,lxoutdir)'
       write(FILENUMBER,*)"      fout=dmkname(xfname(1:lxfname),xoutdir
     &(1:lxoutdir),'.out')"
       write(FILENUMBER,*)"      fout2=dmkname(xfname(1:lxfname),xoutdir
     &(1:lxoutdir),'.fv1')"
       write(FILENUMBER,*)'      IF(LOP.EQ.0) THEN'
       write(FILENUMBER,*)"      open(101,FILE=fout,STATUS='UNKNOWN')"
       write(FILENUMBER,*)"      open(102,FILE=fout2,STATUS='UNKNOWN')"
       write(FILENUMBER,*)'      print*, fout'
       write(FILENUMBER,*)'      print*, fout2'
       write(FILENUMBER,*)'      ENDIF'
       write(FILENUMBER,*)'      IF(LOP.EQ.4) THEN'
       write(FILENUMBER,*)"      open(101,FILE=fout,STATUS='UNKNOWN')"
       write(FILENUMBER,*)"      open(102,FILE=fout2,STATUS='UNKNOWN')"
       write(FILENUMBER,*)'      print*, fout'
       write(FILENUMBER,*)'      print*, fout2'
       write(FILENUMBER,*)'      ENDIF'
       write(FILENUMBER,*)'      IF(LOP.EQ.3) THEN'
       write(FILENUMBER,*)'      AktuellerStep=KSTEP'
       write(FILENUMBER,*)'      AktuellesInk=(KINC-1)'                 !In Version 14.1 KINC immer um 1 zu groß! 
       write(FILENUMBER,*)'      print*, KINC, KSTEP'
       write(FILENUMBER,*)'      print*, aktuellerStep,AktuellesInk'
       write(FILENUMBER,*)'      write(101,*) AktuellesInk'             
       write(FILENUMBER,*)'      close(101)'
       write(FILENUMBER,*)'      if(flag.eq.0) then'
       write(FILENUMBER,*)'      DO I=1,',Elemente,''
       write(FILENUMBER,*)'      write(102,110)I,ARRAY_FV(I,1),ARRAY_FV     !Temperaturen an Integrationspunkt(en) für fv1 Datei, auf Anzahl Integrationspunkte anpassen
     &(I,2),ARRAY_FV(I,3),' 
       write(FILENUMBER,*)'    &ARRAY_FV(I,4)'
       write(FILENUMBER,*)'      END DO'
       write(FILENUMBER,*)'      close(102)'
       write(FILENUMBER,*)'      FLAG=1'
       write(FILENUMBER,*)'      ENDIF'
       write(FILENUMBER,*)'      ENDIF'
       write(FILENUMBER,*)'      RETURN'
       write(FILENUMBER,*)'      END'
c      Maximale Temperaturverteilung       
       write(FILENUMBER,*)'     SUBROUTINE UVARM(UVAR,DIRECT,T,TIME,
     &DTIME,CMNAME,ORNAME,'
       write(FILENUMBER,*)'    &NUVARM,NOEL,NPT,LAYER,KSPT,KSTEP,KINC,
     &NDI,NSHR,COORD,'
       write(FILENUMBER,*)'    &JMAC,JMATYP,MATLAYO,LACCFLA)'
       write(FILENUMBER,*)"      use global"
       write(FILENUMBER,*)"      INCLUDE 'ABA_PARAM.INC'"
       write(FILENUMBER,*)'      CHARACTER*80 CMNAME,ORNAME'
       write(FILENUMBER,*)'      CHARACTER*3 FLGRAY(15)'
       write(FILENUMBER,*)"      DIMENSION UVAR(NUVARM),DIRECT(3,3),
     &T(3,3),TIME(2)"
       write(FILENUMBER,*)'      DIMENSION ARRAY(15),JARRAY(15),
     &JMAC(*),JMATYP(*),COORD(*)'
      write(FILENUMBER,*)"      CALL GETVRM('TEMP',ARRAY,JARRAY,FLGRAY,
     &JRCD,JMAC,JMATYP,"
       write(FILENUMBER,*)"    &MATLAYO,LACCFLA)"
       write(FILENUMBER,*)'      JERROR = JERROR + JRCD'
       write(FILENUMBER,*)'      UVAR(1) = ARRAY(1)'
c       write(FILENUMBER,*)'      TEMP = ARRAY(1)'
       write(FILENUMBER,*)'      ARRAY_FV(NOEL,NPT) = ARRAY(1)'
c       write(FILENUMBER,*)'      print*,NOEL,NPT,ARRAY_FV(NOEL,NPT)'
       write(FILENUMBER,*)'      ARRAY(1:15)=0.'
       write(FILENUMBER,*)'      RETURN'
       write(FILENUMBER,*)'      END'              
       write(FILENUMBER,*)'      character*(*) FUNCTION dmkname
     &(fname,dname,exten)'
       write(FILENUMBER,*)'      character*(*) fname,dname,exten'
       write(FILENUMBER,*)'      ltot = len(fname)'
       write(FILENUMBER,*)'      lf = 0'
       write(FILENUMBER,*)'      DO k1 = ltot,2,-1'
       write(FILENUMBER,*)"       if (lf.EQ.0.AND.fname(k1:k1).NE.' ')
     & lf = k1"
       write(FILENUMBER,*)'      END DO'
       write(FILENUMBER,*)'      ltot = len(dname)'
       write(FILENUMBER,*)'      ld = 0'
       write(FILENUMBER,*)'      DO k1 = ltot,2,-1'
       write(FILENUMBER,*)"       IF (ld.EQ.0.AND.dname(k1:k1).NE.' ')
     &  ld = k1"
       write(FILENUMBER,*)'      END DO'
       write(FILENUMBER,*)'      ltot = len(exten)'
       write(FILENUMBER,*)'      le = 0'
       write(FILENUMBER,*)'      DO k1 = ltot,2,-1'
       write(FILENUMBER,*)"       IF (le.EQ.0.AND.exten(k1:k1).NE.' ')
     &  le = k1"
       write(FILENUMBER,*)'      END DO'
       write(FILENUMBER,*)'      IF ((lf+ld+le).LE.len(dmkname)) THEN'
       write(FILENUMBER,*)"      dmkname=dname(1:ld)//'/'//fname(1:lf)"
       write(FILENUMBER,*)'      ltot = ld + lf + 1'
       write(FILENUMBER,*)'      IF ( le.GT.0) THEN'
       write(FILENUMBER,*)'      dmkname = dmkname(1:ltot)//exten(1:le)'
       write(FILENUMBER,*)'      END IF'
       write(FILENUMBER,*)'      END IF'
       write(FILENUMBER,*)'      RETURN'
       write(FILENUMBER,*)'      END'
C*** Möglichkeit zur Einbindung von Phasenumwandlungen und Berücksichtigung dieser bezüglich der Werkstoffkennwerte     
c       write(FILENUMBER,*)'     SUBROUTINE usdfld(field,statev,pnewdt,
c     &direct,t,celent,time,dtime,'
c       write(FILENUMBER,*)'    &cmname,orname,nfield,nstatv,noel,npt,
c     &layer,kspt,kstep,kinc,'
c       write(FILENUMBER,*)'    &ndi,nshr)'
c       write(FILENUMBER,*)"      use global"
c       write(FILENUMBER,*)"      include 'ABA_PARAM.INC'"
c       write(FILENUMBER,*)"      save"

C*********************************************************************              
c       write(FilENUMBER,*)"      parameter(laz=6,pi =
c     & 3.14159265359,tmpmelt=1600)"  
c       write(FILENUMBER,*)"      character*8 cmname,orname,flgray(15)"
c       write(FILENUMBER,*)"     DIMENSION FIELD(NFIELD),STATEV(NSTATV),"
c       write(FILENUMBER,*)"    &DIRECT(3,3),T(3,3),TIME(2),sdvn(NSTATV)"
c       write(FILENUMBER,*)"     DIMENSION vweld(laz),tflanke(laz),"
c       write(FILENUMBER,*)"    &tfuge(laz),rtorch(laz)"
c       write(FILENUMBER,*)"     DIMENSION yamp(laz),ael(laz),bel(laz),"
c       write(FILENUMBER,*)"    &cel(laz),x(3),ila(laz)"
c       write(FILENUMBER,*)"     DIMENSION ARRAY(15),JARRAY(15)"
c       write(FILENUMBER,*)"     double precision posit"

c       write(FILENUMBER,*)'     field(1)=2.0'
c       write(FILENUMBER,*)'     field(2)=1.0'
c       write(FILENUMBER,*)'     field(3)=1.0'

c       write(FILENUMBER,*)'      RETURN'
c       write(FILENUMBER,*)'      END'

      close(FILENUMBER)
      
C     *****************************************************************
C     *** Aufruf mechanische Rechnung ***      
C     *****************************************************************      

       AufrufMechanischeRechnung='abq2018 cpus=2  inter job='
     &//trim(MECHNAME)//' user='//trim(UVARMNAME)//' scratch=.'

      call system(AufrufMechanischeRechnung)     

c       pause
C     *****************************************************************
C     ***  Erstellung FPERT-Programm zum Auslesen der erstellten
C     Fil-Datei aus der mechanischen Berechnung ***
C        *.015-Datei: Knotenkoordinaten des deformierten Netzes
C        *.016-Datei: Knotenkoordinaten und zugehöriger Kontaktdruck
C        *.017-Datei: Kontaktfläche CAREA
C     Out-Datei aus der mechanischen Berechnung wird benötigt zur Definition
C     des auszulesenden Steps und Inkrement (Hinweis: Fil-File hinterlegt
C     auch bei Restart alle vorangegangene Ergebnisse)
C     *****************************************************************   
      
       FPERTNUMBER=100
       STEP=2

       FpertName='_WPS_FPERT_CAREA_START'
       FpertFileName=trim(Datum)//trim(FPERTName)
       print*, FPERTFILENAME
       FpertFileNameFULL=trim(FpertFileName)//StringEndungFor

        open(FPERTNUMBER, file=trim(FPERTFileNameFull),
     &STATUS='UNKNOWN') 
        open(FPERTNUMBER+1,file=trim(MECHNAME)//'.out',
     &STATUS='UNKNOWN')
        read(FPERTNUMBER+1,*) MECHINK
        print*, 'MECHINK:', MECHINK
        close(FPERTNUMBER+1)
c       PAUSE
       write(FPERTNUMBER,*)'     SUBROUTINE ABQMAIN'
       write(FPERTNUMBER,*)"     INCLUDE 'aba_param.inc'"
       write(FPERTNUMBER,*)'     DIMENSION  ARRAY(513), JRRAY
     &(NPRECD,513),LRUNIT(2,1)'  
       write(FPERTNUMBER,*)'     EQUIVALENCE (ARRAY(1), JRRAY(1,1))'
       write(FPERTNUMBER,*)'     PARAMETER (ITOTAL = ',Knoten,')'
       write(FPERTNUMBER,*)'     DIMENSION DISP(6,ITOTAL),
     & COORD(3,ITOTAL)'
       write(FPERTNUMBER,*)'     DIMENSION CPRESS(ITOTAL)'
       write(FPERTNUMBER,*)'     DOUBLE PRECISION CAREA(1:10)'
       write(FPERTNUMBER,*)'     DIMENSION INODE(ITOTAL), IDOF(30),
     & JEIGNO(10)'
       write(FPERTNUMBER,*)'     DOUBLE PRECISION DISPLACEMENT
     &(1:',Knoten,',1:2)'
       write(FPERTNUMBER,*)'     CHARACTER FNAME*80,OUTFILE*(*)'
       write(FPERTNUMBER,*)'     CHARACTER OUTFILE2*(*)'
       write(FPERTNUMBER,*)'     CHARACTER OUTFILE3*(*)'
       write(FPERTNUMBER,*)"     PARAMETER (OUTFILE = '",
     &trim(MechName),".015')"
       write(FPERTNUMBER,*)"     PARAMETER (OUTFILE2 = '",
     &trim(MechName),".016')"
       write(FPERTNUMBER,*)"     PARAMETER (OUTFILE3 = '",
     &trim(MechName),".017')"
       write(FPERTNUMBER,*)'     ICYCLE = 0'
       write(FPERTNUMBER,*)'     I1901  = 0'
       write(FPERTNUMBER,*)'     I101   = 0'
       write(FPERTNUMBER,*)'     I      = 1'
       write(FPERTNUMBER,*)'     K      = 1'
       write(FPERTNUMBER,*)'     J      = 0'
       write(FPERTNUMBER,*)'     NRU = 1'             
       write(FPERTNUMBER,*)'     LRUNIT(1,NRU) = 8'
       write(FPERTNUMBER,*)'     LRUNIT(2,NRU) = 2'
       write(FPERTNUMBER,*)'     LOUTF = 0'
       write(FPERTNUMBER,*)"     OPEN(UNIT=15,FILE=OUTFILE,STATUS=
     &'UNKNOWN',IOSTAT = J)"
       write(FPERTNUMBER,*)'     IF (J .NE. 0) THEN'
       write(FPERTNUMBER,*)'      WRITE(*,*) OUTFILE'
c       write(FPERTNUMBER,*)'      GOTO 950'
       write(FPERTNUMBER,*)'     END IF'
       write(FPERTNUMBER,*)"     OPEN(UNIT=16,FILE=OUTFILE2,STATUS=
     &'UNKNOWN',IOSTAT = J)"
       write(FPERTNUMBER,*)'     IF (J .NE. 0) THEN'
       write(FPERTNUMBER,*)'      WRITE(*,*) OUTFILE'
c       write(FPERTNUMBER,*)'      GOTO 950'
       write(FPERTNUMBER,*)'     END IF'
       write(FPERTNUMBER,*)"     OPEN(UNIT=17,FILE=OUTFILE3,STATUS=
     &'UNKNOWN',IOSTAT = J)"
       write(FPERTNUMBER,*)'     IF (J .NE. 0) THEN'
       write(FPERTNUMBER,*)'      WRITE(*,*) OUTFILE'
c       write(FPERTNUMBER,*)'      GOTO 950'
       write(FPERTNUMBER,*)'     END IF'
       write(FPERTNUMBER,*)"     FNAME='",trim(MechName),"'"
       write(FPERTNUMBER,*)'     CALL  INITPF
     & (FNAME, NRU, LRUNIT, LOUTF)'
       write(FPERTNUMBER,*)'     JUNIT = LRUNIT(1,NRU)'
       write(FPERTNUMBER,*)'     CALL  DBRNU (JUNIT)'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE (JRCD .EQ. 0)'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,2) .EQ. 1921 ) THEN'       !Record type: Abaqus release, etc.
       write(FPERTNUMBER,*)'     NODEMAX = JRRAY(1,8)'                  !	6  –  Number of nodes in the model.
       write(FPERTNUMBER,*)'     IELMAX  = JRRAY(1,7)'                  !	5  –  Number of elements in the model
       write(FPERTNUMBER,*)'     ICYCLE = ICYCLE +1'
       write(FPERTNUMBER,*)'     ENDIF'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     ENDDO'
       write(FPERTNUMBER,*)'     icycle=1'
       write(FPERTNUMBER,*)'     CALL DBFILE (2, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     WRITE(*,*) NODEMAX, IELMAX'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE (JRCD .EQ. 0)'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,2) .EQ. 1901 .AND.         !Record type: Node definitions
     & ICYCLE .LE. 1) THEN'
       write(FPERTNUMBER,*)'     I1901 = I1901 + 1'
       write(FPERTNUMBER,*)'     INODE(I1901)  = JRRAY(1,3)'            !1  –  Node number.
       write(FPERTNUMBER,*)'     COORD(1,I1901) = ARRAY(4)'             !2  –  First coordinate.
       write(FPERTNUMBER,*)'     COORD(2,I1901) = ARRAY(5)'             !3  –  Second coordinate.
       write(FPERTNUMBER,*)'     COORD(3,I1901) = 0.0D0'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,1) .GE. 6) COORD(3,I1901)
     & = ARRAY(6)'
       write(FPERTNUMBER,*)'     ELSE IF (JRRAY(1,2) .EQ. 1902) THEN'   !Record type: Active degrees of freedom
       write(FPERTNUMBER,*)'     DO 15 IXX = 1, JRRAY(1,1)-2'
       write(FPERTNUMBER,*)'     IDOF(IXX) = JRRAY(1,IXX+2)'
       write(FPERTNUMBER,*)'15   CONTINUE'
       write(FPERTNUMBER,*)'     ITRANS = 3'
       write(FPERTNUMBER,*)'     IF (IDOF(3) .EQ. 0) ITRANS = 2'
       write(FPERTNUMBER,*)'     end if'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     END DO'
       write(FPERTNUMBER,*)'     CALL DBFILE (2, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     icycle=1'
       write(FPERTNUMBER,*)'     call posfil(',(STEP+1),',',
     &(MECHINK), 
     &',ARRAY,JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE(JRCD.eq.0)'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,2) .EQ. 101.and.           !101=>Components of displacement
     &icycle.le.1) THEN'  
       write(FPERTNUMBER,*)'     I101 = I101 + 1'
       write(FPERTNUMBER,*)'     DISP(1,I101) = ARRAY(4)'
       write(FPERTNUMBER,*)'     DISP(2,I101) = ARRAY(5)'
       write(FPERTNUMBER,*)'     IF (INODE(I101) .EQ. 0) INODE(I101) =
     & JRRAY(1,3)'
       write(FPERTNUMBER,*)'     if(i101.eq.NODEMAX) then'
       write(FPERTNUMBER,*)'     ICYCLE=ICYCLE+1'
       write(FPERTNUMBER,*)'     CALL NODEGEN(COORD,DISP,I1901,FACTOR)'
       write(FPERTNUMBER,*)'     ENDIF '
       write(FPERTNUMBER,*)'     ENDIF'
       write(FPERTNUMBER,*)'     CALL DBFILE(0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     ENDDO'
!      27.01. Ausgabe von CAREA und CPRESS
       write(FPERTNUMBER,*)'     CALL DBFILE(2, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     icycle=1'
       write(FPERTNUMBER,*)'     CPRESS=0.d0'
       write(FPERTNUMBER,*)'     call posfil(',(STEP+1),',',
     &(MECHINK), !27.01.09 Hier muss geprüft werden
     &',ARRAY,JRCD)'
       write(FPERTNUMBER,*)'     print*, JRCD'
       write(FPERTNUMBER,*)'     DO WHILE (JRCD .EQ. 0)'
       write(FPERTNUMBER,*)'     IF(JRRAY(1,2) .EQ. 1504 .AND. ICYCLE       !1504=>Number of traction components (2 for 2-D or axisymmetric cases, 3 for 3-D cases).
     & .LE. 1) THEN'
       write(FPERTNUMBER,*)'     I1511=JRRAY(1,3)'
       write(FPERTNUMBER,*)'     ELSE IF(JRRAY(1,2) .EQ. 1511 .AND.
     & ICYCLE.LE. 1) THEN'
       write(FPERTNUMBER,*)'     CPRESS(I1511) = ARRAY(3)'                  !Contact pressure between the node on the slave surface and the master surface with which it interacts.
       write(FPERTNUMBER,*)'     end if'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     end do'
       write(FPERTNUMBER,*)'     CALL DBFILE(2, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     icycle=1'
       write(FPERTNUMBER,*)'     CAREA=0.d0'
       write(FPERTNUMBER,*)'     i=1'
       write(FPERTNUMBER,*)'     call posfil(',(STEP+1),',',
     &(MECHINK), !27.01.09 Hier muss geprüft werden
     &',ARRAY,JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE (JRCD .EQ. 0)'
       write(FPERTNUMBER,*)'     IF(JRRAY(1,2) .EQ. 1503 .AND. ICYCLE   !1503
     & .LE. 1) THEN'
       write(FPERTNUMBER,*)'     ELSE IF(JRRAY(1,2) .EQ. 1524 .AND.                                                       !ELSE IF(JRRAY(1,2) .EQ. 1524 .AND.                 !1524...total area in contact
     & ICYCLE .LE. 1) THEN'
       write(FPERTNUMBER,*)'     CAREA(i) = ARRAY(3)'
       write(FPERTNUMBER,*)'     i=i+1'
       write(FPERTNUMBER,*)'     END IF'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     end do'
       write(FPERTNUMBER,*)'     WRITE(*,100) OUTFILE'
       write(FPERTNUMBER,*)"100  FORMAT(//,2X,'The perturbed mesh
     & data are being written to:',"
       write(FPERTNUMBER,*)'    &1X,A,//)'
       write(FPERTNUMBER,*)'     DO K = 1, NODEMAX' 
       write(FPERTNUMBER,*)'     WRITE(15,110) INODE(K), (COORD(J,K),J
     & = 1, ITRANS)'
       write(FPERTNUMBER,*)'     WRITE(16,110) K, COORD(1,K),
     &COORD(2,K), CPRESS(K)'
       write(FPERTNUMBER,*)"110  FORMAT(I6,3(',',1PE14.6))"    
       write(FPERTNUMBER,*)'     ENDDO' 
       write(FPERTNUMBER,*)'     DO I=1,6'
       write(FPERTNUMBER,*)'     WRITE(17,*)i, CAREA(i)'
       write(FPERTNUMBER,*)'     ENDDO'
       write(FPERTNUMBER,*)'     CLOSE (15)'
       write(FPERTNUMBER,*)'     CLOSE (16)'
       write(FPERTNUMBER,*)'     CLOSE (17)' 
       write(FPERTNUMBER,*)'     WRITE(*,120) ' 
       write(FPERTNUMBER,*)"120  FORMAT(//,2X,' . . . PROGRAM FINISHED
     & SUCCESSFULLY . . . ')"
       write(FPERTNUMBER,*)'     RETURN'
       write(FPERTNUMBER,*)'     END'
       write(FPERTNUMBER,*)'     SUBROUTINE NODEGEN
     &(COORD,DISP,I1901,FACTOR)'
       write(FPERTNUMBER,*)"     INCLUDE 'aba_param.inc'"
       write(FPERTNUMBER,*)'     DIMENSION COORD(3,*),DISP(6,*)'
       write(FPERTNUMBER,*)'     DO I = 1, I1901'
       write(FPERTNUMBER,*)'     COORD(1,I) = COORD(1,I) +  DISP(1,I)'
c       write(FPERTNUMBER,*)'     print*, I, COORD(1,I), DISP(1,I)'
c       write(FPERTNUMBER,*)'     pause'
       write(FPERTNUMBER,*)'     if(COORD(1,I).lt.0.d0) then'
       write(FPERTNUMBER,*)'     COORD(1,I)=0.d0'
       write(FPERTNUMBER,*)'     end if'
       write(FPERTNUMBER,*)'     COORD(2,I) = COORD(2,I) +  DISP(2,I)'
       write(FPERTNUMBER,*)'     COORD(3,I) = COORD(3,I) +  DISP(3,I)'
       write(FPERTNUMBER,*)'     END DO'
       write(FPERTNUMBER,*)'     RETURN'
       write(FPERTNUMBER,*)'900  FORMAT(//,'
       write(FPERTNUMBER,*)"    &/,2X,'TROUBLE OPENING FILE',1X,A)"
       write(FPERTNUMBER,*)'950  WRITE(*,1000)'
       write(FPERTNUMBER,*)'1000 FORMAT(//,'
       write(FPERTNUMBER,*)"    &/,2X,' . . . TROUBLE READING DATA . 
     &. .   ',"
       write(FPERTNUMBER,*)"    &/,2X,' . . .   PROGRAM STOPPED    . .
     & .   ',/)"
       write(FPERTNUMBER,*)'2000 FORMAT(//,'
       write(FPERTNUMBER,*)"    & /,'   +-----------------+',"
       write(FPERTNUMBER,*)"    & /,'   +------- ---------+',//)"
       write(FPERTNUMBER,*)"2010 FORMAT(//,"       
       write(FPERTNUMBER,*)"    &/,2X,'Nodes. . . . . . . ',I5,"
       write(FPERTNUMBER,*)"    &/,2X,'Elements   . . . . . . . ',I5)"
       write(FPERTNUMBER,*)'     RETURN'
       write(FPERTNUMBER,*)'     END'
       close(FPERTNUMBER) 

C     *****************************************************************
C     *** Aufruf Auslesen der Fil-Datei ***      
C     *****************************************************************
       AufrufFPERT='abq2018 make job=' // trim(FPERTFILENAME)
       call system(AufrufFpert) 

       AufrufFPERT2='abq2018  ' // trim(FPERTFILENAME)

       call system(AufrufFpert2)    

       call system(AufrufFpert2)

c        pause     

C****** Iterative Bestimmung des spezifischen Kontaktleitwertes***********
      
        k=0
        STRINGENDUNGEPOT='.015'      
        EPOTNAME=trim(ELEKNAME)//StringEndungEPOT
        EPOTVALUE=0.d0

C      Übergabe-Experiment
C     *****************************************************************
C     *** Einlesen der gemessenen Potentialdifferenzen und Ermittlung
C     der Potentialdifferenzen der Übergange Elektrode-Blech,
C     Blech-Blech und Blech-Elektrode ***      
C     *****************************************************************  
       EPOTVALUE(k,1)=EPOTVALUEEXPERIMENT(1,1)
       EPOTVALUE(k,2)=EPOTVALUEEXPERIMENT(1,2) 
       EPOTVALUE(k,3)=EPOTVALUEEXPERIMENT(1,3)
       EPOTVALUE(k,4)=EPOTVALUEEXPERIMENT(1,4)
        
       EPOTDIFFERENZ(k,1)=EPOTVALUE(k,1)-EPOTVALUE(k,4)
       EPOTDIFFERENZ(k,2)=EPOTVALUE(k,2)-EPOTVALUE(k,4)
       EPOTDIFFERENZ(k,3)=EPOTVALUE(k,3)-EPOTVALUE(k,4)

       print*, EPOTVALUE(k,1), EPOTVALUE(k,2),EPOTVALUE(k,3),
     &EPOTVALUE(k,4) 



C     Interne Schleife zur Bestimmung des Kontaktleitwerts

       

       IntegerIterationElekNumber=1
c       write(a,*)IntegerIterationElekNumber
c       a=adjustl(a)
c       StringIterationElekNumber=a

       ITNUMBER=40001
       FPERTNUMBER=50001

C***********Einlesen der mechanischen Kontaktfläche**************
C*** Aufgrund der doppelten Definition der Kontaktflächen exisiteren bei
C*** einer Zweiblechschweißung 6 Kontaktflächen *** 
      
        FILE_KONTAKTFLAECHE=trim(Datum)//trim(MECH)//'.017'
        open(42,file=FILE_KONTAKTFLAECHE, status='UNKNOWN')
        do kontakt=1,6
        read(42,*) kk,KONTAKTFLAECHE(kontakt)
        print*, kk,KONTAKTFLAECHE(kontakt)
        end do !kontakt
        close(42)
C*****************************************************************        

C      Vorgabe des 1. Kontaktleitwerts, hier: unendlich
 
       LEITWERT_ELBL1_SPEZ(0)=1000000000000.d0
       LEITWERT_ELBL2_SPEZ(0)=1000000000000.d0 
       LEITWERT_BLBL_SPEZ(0)=1000000000000.d0  

       do k=1,10

       write(a,*)IntegerIterationElekNumber
       a=adjustl(a)
       StringIterationElekNumber=a

       IterationElekName='_WPS_ELEK_Iteration'
       IterationElekFileName=trim(Datum)//trim(IterationElekName)//
     &trim(StringIterationElekNumber)
       print*, IterationElekFileName
       IterationElekFileName=adjustl(IterationElekFileName)
        IterationElekFileNameFull=trim(IterationElekFileName)//
     &trim(StringEndungInp)
       print*, IterationElekFileNameFull

C     *****************************************************************
C     ***  Erstellen der Abaqus-Input-Datei (elektrisch-thermisch) ***        
C     *****************************************************************  

       open(ITNUMBER, file=trim(IterationELEKFileNameFull),
     &STATUS='UNKNOWN') 
       
        write(ITNUMBER,*)'*Heading'
        write(ITNUMBER,*)'*Preprint, echo=NO, model=NO, history=NO
     &, contact=YES'
        write(ITNUMBER,*)'*Heading'
c        write(ITNUMBER,*)'*Part, name=Part-1'
        write(ITNUMBER,*)'*Node, nset=NALL'
C     *****************************************************************
C     ***  Einlesen des deformierten Netzes aus der durchgeführten 
C          mechanischen Berechnung ***        
C     *****************************************************************       
       write(ITNUMBER,*)'*INCLUDE, input=',trim(MechName),
     &'.015'
c     *****************************************************************
C     ***  Elementdefiniton - Vorgabe ***       
        write(ITNUMBER,*)'*INCLUDE, input=WPS_ELEK_ELEMENTE.txt'
c     *****************************************************************
C     ***  Kontakt- und Materialdefinition ***  
       write(ITNUMBER,*)'**'
       write(ITNUMBER,*)'*INCLUDE, input=WPS_ELEK_Contact.txt'
c     *****************************************************************
C     ***  Randbedingungen *** 
c     *****************************************************************         
       write(ITNUMBER,*)'*Initial Conditions, type=TEMPERATURE'
       write(ITNUMBER,*)'NALL, 20.'
       write(ITNUMBER,*)'*STEP,INC=10000'
       write(ITNUMBER,*)'*COUPLED THERMAL-ELECTRICAL, deltmx=5000'
       write(ITNUMBER,*)'0.0001, 0.001, , ,0.001'
       write(ITNUMBER,*)'*FIELD,variable=1,USER'
       write(ITNUMBER,*)'NALL'
       write(ITNUMBER,*)'*BOUNDARY'
       write(ITNUMBER,*)'BC_Elektrode, 9, , 0.0'
       write(ITNUMBER,*)'LOAD_ELEKTRODE, 11, ,20'
       write(ITNUMBER,*)'LOAD_ELEKTRODE_REFERENZ, 11, ,20'
       write(ITNUMBER,*)'BC_Elektrode, 11, , 20'
       write(ITNUMBER,*)'BC_Temp, 11, ,20'
       write(ITNUMBER,*)'*Dsecurrent'
       write(ITNUMBER,*)'SURF_LOAD_CURRENT,CS,', Strom(1)
       write(ITNUMBER,*)'*OUTPUT, FIELD'
       write(ITNUMBER,*)'*ELEMENT OUTPUT'
       write(ITNUMBER,*)'HFL,ECD,EPG,JENER'!, FV'
       write(ITNUMBER,*)'*NODE OUTPUT'
       write(ITNUMBER,*)'NT,EPOT'
C     ******** FIL-Datei zur Ermittlung des elektrischen Potentials****          
       write(ITNUMBER,*)'*NODE FILE'
       write(ITNUMBER,*)'EPOT'
       write(ITNUMBER,*)'*OUTPUT,HISTORY'
       write(ITNUMBER,*)'*ENERGY OUTPUT, VARIABLE=PRESELECT'
       write(ITNUMBER,*)'*END STEP'
       close(ITNUMBER)
C     *****************************************************************
C     ***  Subroutine zur Definiton des elektrischen Kontaktwiderstands
C     ***  Schnittstelle zu Kontaktdruck und Kontaktfläche zur möglichen
C     Einbindung von kontaktdruckabhängigen Formulierungen       
C     *****************************************************************
      
       IterationElekNameFIELD='_WPS_FIELD'
       IterationElekFileNameFIELD=trim(Datum)//'_0'//
     &trim(IterationElekNameFIELD)//
     &trim(StringIterationElekNumber)
       print*, IterationElekFileNameFIELD
       IterationElekFileNameFIELD=adjustl(IterationElekFileNameFIELD)
       IterationElekFileNameFullFIELD=trim(IterationElekFileNameFIELD)//
     &trim(StringEndungFor)
       print*, IterationElekFileNameFullFIELD


      open(ITNUMBER+1, file=trim(IterationELEKFileNameFullFIELD),
     &STATUS='UNKNOWN')

      write(ITNUMBER+1,*)'     module global'
      write(ITNUMBER+1,*)'     DOUBLE PRECISION ARRAY(1:',Knoten,'),
     & CAREA(1:3)'
      write(ITNUMBER+1,*)'     INTEGER SURFACE(1:3)'
      write(ITNUMBER+1,*)'     SAVE ARRAY, CAREA,SURFACE'
      write(ITNUMBER+1,*)'     end module' 
      write(ITNUMBER+1,*)'     SUBROUTINE UFIELD(FIELD,KFIELD,NSECPT,
     &KSTEP,KINC,TIME,NODE,'
      write(ITNUMBER+1,*)'    1 COORDS,TEMP,DTEMP,NFIELD)'    
      write(ITNUMBER+1,*)'     use global' 
      write(ITNUMBER+1,*)"     INCLUDE 'ABA_PARAM.INC'" 
      write(ITNUMBER+1,*)'     DIMENSION FIELD(NSECPT,NFIELD), TIME(2)
     &, COORDS(3),' 
      write(ITNUMBER+1,*)'    1 TEMP(NSECPT), DTEMP(NSECPT)' 
      write(ITNUMBER+1,*)'     INTEGER Initflag,I,J' 
      write(ITNUMBER+1,*)'     CHARACTER INFILE*255' 

       write(ITNUMBER+1,*)'      CHARACTER INFILE2*255'
       write(ITNUMBER+1,*)'      CHARACTER*256 JOBNAME'
       write(ITNUMBER+1,*)'      CHARACTER xoutdir*255, xfname*80'
       write(ITNUMBER+1,*)'      CHARACTER dmkname*255, fout*255'

      
      write(ITNUMBER+1,*)'     SAVE Initflag,KNOTEN,XCOORD,YCOORD' 
      write(ITNUMBER+1,*)'     INTEGER KNOTEN(1:',Knoten,')'
      write(ITNUMBER+1,*)'     DOUBLE PRECISION XCOORD(1:',Knoten,'),
     & YCOORD(1:',Knoten,')' 
      write(ITNUMBER+1,*)'      call GETJOBNAME( xfname, lxfname )'
      write(ITNUMBER+1,*)'      call GETOUTDIR( xoutdir,lxoutdir)'
      write(ITNUMBER+1,*)"      fout=dmkname(xfname(1:lxfname),xoutdir
     &(1:lxoutdir),'.out')" 

C********************************
C********************************
      write(ITNUMBER+1,*)"      INFILE=trim(fout)//'/",
     &trim(MECHNAME),"'//'.016'"
      write(ITNUMBER+1,*)"      INFILE2=trim(fout)//'/",
     &trim(MECHNAME),"'//'.017'"
C********************************
      write(ITNUMBER+1,*)'     if(Initflag.eq.0) then' 
      write(ITNUMBER+1,*)"     OPEN(UNIT=16,FILE=INFILE,
     &STATUS='UNKNOWN')" 
      write(ITNUMBER+1,*)'     Do I=1,',Knoten,'' 
      write(ITNUMBER+1,*)'     read(16,*) KNOTEN(I), XCOORD(I),
     & YCOORD(I), ARRAY(I)' 
      write(ITNUMBER+1,*)'     end do' 
      write(ITNUMBER+1,*)"     OPEN(UNIT=17,FILE=INFILE2,
     &STATUS='UNKNOWN')" 
      write(ITNUMBER+1,*)'     DO J=1,3' 
      write(ITNUMBER+1,*)'     read(17,*)SURFACE(J), CAREA(J)' 
      write(ITNUMBER+1,*)'     END DO' 
      write(ITNUMBER+1,*)'     close(16)' 
      write(ITNUMBER+1,*)'     close(17)'
      write(ITNUMBER+1,*)'     Initflag=1' 
      write(ITNUMBER+1,*)'     END IF' 
      write(ITNUMBER+1,*)'     FIELD(1,1)=ARRAY(NODE)'
      write(ITNUMBER+1,*)'     RETURN'
      write(ITNUMBER+1,*)'     END'
      write(ITNUMBER+1,*)'     SUBROUTINE GAPELECTR(SIGMA,D,TEMP,
     &PREDEF,TIME,CINAME,'
      write(ITNUMBER+1,*)'    1 SLNAME,MSNAME,COORDS,NODE,NPRED,KSTEP
     &,KINC)'   
      write(ITNUMBER+1,*)'     use global'
      write(ITNUMBER+1,*)"     INCLUDE 'ABA_PARAM.INC'"
      write(ITNUMBER+1,*)'     CHARACTER*80 CINAME,SLNAME,MSNAME'
      write(ITNUMBER+1,*)'     DIMENSION SIGMA(3),TEMP(2),PREDEF(2,*)
     &,TIME(2),COORDS(2,3)'
      write(ITNUMBER+1,*)'     If(ARRAY(NODE).gt.0.d0.OR.ARRAY(NODE)
     &.LT.0.d0) then'
      write(ITNUMBER+1,*)"     IF(CINAME.eq.'ELEKTRODE') then "
      write(ITNUMBER+1,*)'     SIGMA(1)=',LEITWERT_ELBL1_SPEZ(k-1)
      write(ITNUMBER+1,*)"     else if(CINAME.eq.'BLECH') then"
      write(ITNUMBER+1,*)'     SIGMA(1)=',LEITWERT_BLBL_SPEZ(k-1)
      write(ITNUMBER+1,*)'     END IF'
      write(ITNUMBER+1,*)'      ELSE'
      write(ITNUMBER+1,*)'     SIGMA(1)=0.d0'
      write(ITNUMBER+1,*)'     end if'      
      write(ITNUMBER+1,*)'     RETURN'
      write(ITNUMBER+1,*)'     END'
C******************************************************************************
       write(ITNUMBER+1,*)'      character*(*) FUNCTION dmkname
     &(fname,dname,exten)'
       write(ITNUMBER+1,*)'      character*(*) fname,dname,exten'
       write(ITNUMBER+1,*)'      ltot = len(fname)'
       write(ITNUMBER+1,*)'      lf = 0'
       write(ITNUMBER+1,*)'      DO k1 = ltot,2,-1'
       write(ITNUMBER+1,*)"       if (lf.EQ.0.AND.fname(k1:k1).NE.' ')
     & lf = k1"
       write(ITNUMBER+1,*)'      END DO'
       write(ITNUMBER+1,*)'      ltot = len(dname)'
       write(ITNUMBER+1,*)'      ld = 0'
       write(ITNUMBER+1,*)'      DO k1 = ltot,2,-1'
       write(ITNUMBER+1,*)"       IF (ld.EQ.0.AND.dname(k1:k1).NE.' ')
     &  ld = k1"
       write(ITNUMBER+1,*)'      END DO'
       write(ITNUMBER+1,*)'      ltot = len(exten)'
       write(ITNUMBER+1,*)'      le = 0'
       write(ITNUMBER+1,*)'      DO k1 = ltot,2,-1'
       write(ITNUMBER+1,*)"       IF (le.EQ.0.AND.exten(k1:k1).NE.' ')
     &  le = k1"
       write(ITNUMBER+1,*)'      END DO'
       write(ITNUMBER+1,*)'      IF ((lf+ld+le).LE.len(dmkname)) THEN'
c       write(ITNUMBER+1,*)"      dmkname=dname(1:ld)//'/'//
c     &fname(1:lf)"
       write(ITNUMBER+1,*)"      dmkname=dname(1:ld)"
       write(ITNUMBER+1,*)'      ltot = ld + lf + 1'
       write(ITNUMBER+1,*)'      IF ( le.GT.0) THEN'
c       write(ITNUMBER+1,*)'      dmkname = dmkname(1:ltot)//
c     &exten(1:le)'
       write(ITNUMBER+1,*)'      dmkname = dmkname(1:ltot)'
       write(ITNUMBER+1,*)'      END IF'
       write(ITNUMBER+1,*)'      END IF'
       write(ITNUMBER+1,*)'      RETURN'
       write(ITNUMBER+1,*)'      END'

C     *****************************************************************
C     *** Aufruf elektrisch-thermische Rechnung (Iteration) ***      
C     *****************************************************************
       
        AufrufElektrischeRechnung='abq2018 cpus=2  inter job='
     &//trim(IterationElekFileName)//' user='//
     &trim(IterationElekFileNameFIELD)//' scratch=.'

c       IterationElekFileNameOld=trim(RestartElekfileName)
       print*, AufrufElektrischeRechnung
       call system(AufrufElektrischeRechnung)
C     *****************************************************************
C     ***  Erstellung FPERT-Programm zum Auslesen der erstellten
C     Fil-Datei aus der elektrisch-thermischen Berechnung ***
C        *.015-Datei: Knotenkoordinaten des deformierten Netzes und
C        zugehöriges elektrisches Potential
C     *****************************************************************

        FpertName='_WPS_FPERT_EPOT'
       FpertFileName=trim(Datum)//trim(FPERTName)//'0_'//
     &trim(StringIterationElekNumber)
       FPERTNUMBER=FPERTNUMBER+1
       print*, FPERTFILENAME
       STRINGENDUNGFOR='.for'
       FpertFileNameFULL=trim(FpertFileName)//StringEndungFor

        open(FPERTNUMBER, file=trim(FPERTFileNameFull),
     &STATUS='UNKNOWN')

       open(FPERTNUMBER, file=trim(FPERTFileNameFull),
     &STATUS='UNKNOWN') 
       write(FPERTNUMBER,*)'     SUBROUTINE ABQMAIN'
       write(FPERTNUMBER,*)"     INCLUDE 'aba_param.inc'"
       write(FPERTNUMBER,*)'     DIMENSION  ARRAY(513), JRRAY
     &(NPRECD,513),LRUNIT(2,1)'  
       write(FPERTNUMBER,*)'     EQUIVALENCE (ARRAY(1), JRRAY(1,1))'
       write(FPERTNUMBER,*)'     PARAMETER (ITOTAL = ',Knoten,')'
       write(FPERTNUMBER,*)'     DIMENSION DISP(6,ITOTAL),
     & COORD(3,ITOTAL)'
       write(FPERTNUMBER,*)'     DIMENSION EPOT(ITOTAL)'
       write(FPERTNUMBER,*)'     DOUBLE PRECISION CAREA(1:10)'
       write(FPERTNUMBER,*)'     DIMENSION INODE(ITOTAL), IDOF(30),
     & JEIGNO(10)'
       write(FPERTNUMBER,*)'     DOUBLE PRECISION DISPLACEMENT
     &(1:',Knoten,',1:2)'
       write(FPERTNUMBER,*)'     CHARACTER FNAME*80,OUTFILE*(*)'
       write(FPERTNUMBER,*)"     PARAMETER (OUTFILE = '",
     &trim(IterationElekFileName),".015')"
       write(FPERTNUMBER,*)'     ICYCLE = 0'
       write(FPERTNUMBER,*)'     I1901  = 0'
       write(FPERTNUMBER,*)'     I101   = 0'
       write(FPERTNUMBER,*)'     I      = 1'
       write(FPERTNUMBER,*)'     K      = 1'
       write(FPERTNUMBER,*)'     J      = 0'
       write(FPERTNUMBER,*)'     NRU = 1'             
       write(FPERTNUMBER,*)'     LRUNIT(1,NRU) = 8'
       write(FPERTNUMBER,*)'     LRUNIT(2,NRU) = 2'
       write(FPERTNUMBER,*)'     LOUTF = 0'
       write(FPERTNUMBER,*)"     OPEN(UNIT=15,FILE=OUTFILE,STATUS=
     &'UNKNOWN',IOSTAT = J)"
       write(FPERTNUMBER,*)'     IF (J .NE. 0) THEN'
       write(FPERTNUMBER,*)'      WRITE(*,*) OUTFILE'
c       write(FPERTNUMBER,*)'      GOTO 950'
       write(FPERTNUMBER,*)'     END IF'      
       write(FPERTNUMBER,*)"     FNAME='",trim(IterationElekFileName),"
     &'"
       write(FPERTNUMBER,*)'     CALL  INITPF
     & (FNAME, NRU, LRUNIT, LOUTF)'
       write(FPERTNUMBER,*)'     JUNIT = LRUNIT(1,NRU)'
       write(FPERTNUMBER,*)'     CALL  DBRNU (JUNIT)'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE (JRCD .EQ. 0)'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,2) .EQ. 1921 ) THEN'
       write(FPERTNUMBER,*)'     NODEMAX = JRRAY(1,8)'
       write(FPERTNUMBER,*)'     IELMAX  = JRRAY(1,7)'
       write(FPERTNUMBER,*)'     ICYCLE = ICYCLE +1'
       write(FPERTNUMBER,*)'     ENDIF'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     ENDDO'
       write(FPERTNUMBER,*)'     icycle=1'
       write(FPERTNUMBER,*)'     CALL DBFILE (2, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     WRITE(*,*) NODEMAX, IELMAX'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE (JRCD .EQ. 0)'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,2) .EQ. 1901 .AND.
     & ICYCLE .LE. 1) THEN'
       write(FPERTNUMBER,*)'     I1901 = I1901 + 1'
       write(FPERTNUMBER,*)'     INODE(I1901)  = JRRAY(1,3)'
       write(FPERTNUMBER,*)'     COORD(1,I1901) = ARRAY(4)'
       write(FPERTNUMBER,*)'     COORD(2,I1901) = ARRAY(5)'
       write(FPERTNUMBER,*)'     COORD(3,I1901) = 0.0D0'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,1) .GE. 6) COORD(3,I1901)
     & = ARRAY(6)'
       write(FPERTNUMBER,*)'     ELSE IF (JRRAY(1,2) .EQ. 1902) THEN'
       write(FPERTNUMBER,*)'     DO 15 IXX = 1, JRRAY(1,1)-2'
       write(FPERTNUMBER,*)'     IDOF(IXX) = JRRAY(1,IXX+2)'
       write(FPERTNUMBER,*)'15   CONTINUE'
       write(FPERTNUMBER,*)'     ITRANS = 3'
       write(FPERTNUMBER,*)'     IF (IDOF(3) .EQ. 0) ITRANS = 2'
       write(FPERTNUMBER,*)'     end if'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     END DO'
       write(FPERTNUMBER,*)'     CALL DBFILE (2, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     icycle=1'
       write(FPERTNUMBER,*)'     call posfil(1,6,ARRAY,JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE(JRCD.eq.0)'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,2) .EQ. 105.and.
     &icycle.le.1) THEN'  
       write(FPERTNUMBER,*)'     I105 = JRRAY(1,3)'
       write(FPERTNUMBER,*)'     EPOT(I105)=ARRAY(4)'
       write(FPERTNUMBER,*)'     END IF'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     END DO'
       write(FPERTNUMBER,*)'     WRITE(*,100) OUTFILE'
       write(FPERTNUMBER,*)"100  FORMAT(//,2X,'The perturbed mesh
     & data are being written to:',"
       write(FPERTNUMBER,*)'    &1X,A,//)'
       write(FPERTNUMBER,*)'     DO K = 1, NODEMAX' 
       write(FPERTNUMBER,*)'     WRITE(15,110) INODE(K), (COORD(J,K),J
     & = 1, 3),'
       write(FPERTNUMBER,*)'    1 EPOT(INODE(K))'
       write(FPERTNUMBER,*)"110  FORMAT(I6,4(',',1PE14.6))"    
       write(FPERTNUMBER,*)'     ENDDO' 
       write(FPERTNUMBER,*)'     CLOSE (15)'
       write(FPERTNUMBER,*)'     WRITE(*,120) ' 
       write(FPERTNUMBER,*)"120  FORMAT(//,2X,' . . . PROGRAM FINISHED
     & SUCCESSFULLY . . . ')"
       write(FPERTNUMBER,*)'     RETURN'
       write(FPERTNUMBER,*)'     END'
       close(FPERTNUMBER)

C     *****************************************************************
C     *** Aufruf Auslesen der Fil-Datei (elektrisch-thermisch) ***      
C     *****************************************************************
       AufrufFPERT='abq2018 make job=' // trim(FPERTFILENAME)

       call system(AufrufFpert) 

       AufrufFPERT2='abq2018 ' // trim(FPERTFILENAME)

       call system(AufrufFpert2) 

      ITNUMBER=ITNUMBER+2
      FPERTNUMBER=Fpertnumber+2      
      IntegerIterationElekNumber=IntegerIterationElekNumber+1


              STRINGENDUNGEPOT='.015'      
        EPOTNAME=trim(IterationElekFileName)//StringEndungEPOT
c        EPOTVALUE=0.d0
        open(123,file=EPOTNAME, status='UNKNOWN')

        do i=1,Knoten
        read(123,*)EPOTNODEAKTUELL, EPOTCOORDAKTUELL(1),
     & EPOTCOORDAKTUELL(2), EPOTCOORDAKTUELL(3), EPOTVALUEAKTUELL
C****** Knoten der Spannungsabgriffe**********************************
C       modellspezifisch zu definieren 
        if(EPOTNODEAKTUELL.EQ.Spannungsabgriff1) then
                EPOTVALUE(k,4)=EPOTVALUEAKTUELL
        else if (EPOTNODEAKTUELL.EQ.Spannungsabgriff2) then       
                EPOTVALUE(k,3)=EPOTVALUEAKTUELL
        else if (EPOTNODEAKTUELL.EQ.Spannungsabgriff3) then       
                EPOTVALUE(k,2)=EPOTVALUEAKTUELL
        else if (EPOTNODEAKTUELL.EQ.Spannungsabgriff4) then       
                EPOTVALUE(k,1)=EPOTVALUEAKTUELL
        end if 
C**********************************************************************      

       end do
       EPOTDIFFERENZ(k,1)=EPOTVALUE(k,1)-EPOTVALUE(k,4)
       EPOTDIFFERENZ(k,2)=EPOTVALUE(k,2)-EPOTVALUE(k,4)
       EPOTDIFFERENZ(k,3)=EPOTVALUE(k,3)-EPOTVALUE(k,4)

      print*, EPOTVALUE(k,1), EPOTVALUE(k,2),EPOTVALUE(k,3),
     &EPOTVALUE(k,4)  
c      PAUSE     
C       Qualitätsüberprüfung des verwendeten Kontaktleitwertes

       EPOTDIFFERENZ_ELBL1(k)=EPOTVALUE(k,1)-EPOTVALUE(k,2)
       EPOTDIFFERENZ_BLBL(k) =EPOTVALUE(k,2)-EPOTVALUE(k,3)  
       EPOTDIFFERENZ_ELBL2(k)=EPOTVALUE(k,3)-EPOTVALUE(k,4)

C      Temporär Vorgabe hier zu berechnen       

       EPOTDIFFERENZ_ELBL1_VORGABE=EPOTVALUE(0,1)-EPOTVALUE(0,2)
       EPOTDIFFERENZ_BLBL_VORGABE=EPOTVALUE(0,2)-EPOTVALUE(0,3)  
       EPOTDIFFERENZ_ELBL2_VORGABE=EPOTVALUE(0,3)-EPOTVALUE(0,4) 

       print*,EPOTDIFFERENZ_ELBL1_VORGABE
       print*, EPOTDIFFERENZ_BLBL_VORGABE
       print*,EPOTDIFFERENZ_ELBL2_VORGABE
c       PAUSE
       QUALITAET_ELBL1(k)= EPOTDIFFERENZ_ELBL1_VORGABE-
     &EPOTDIFFERENZ_ELBL1(k)
       QUALITAET_BLBL(k)=EPOTDIFFERENZ_BLBL_VORGABE-
     &EPOTDIFFERENZ_BLBL(k)
       QUALITAET_ELBL2(k)=EPOTDIFFERENZ_ELBL2_VORGABE-
     &EPOTDIFFERENZ_ELBL2(k)

       print*, QUALITAET_ELBL1(k),EPOTDIFFERENZ_ELBL1_VORGABE,
     &EPOTDIFFERENZ_ELBL1(k) 
       print*, QUALITAET_BLBL(k)
       print*, QUALITAET_ELBL2(k)
c       PAUSE

C       Abfrage, ob Iteration i.O.
        
        SUMME_QUALITAET(k)=abs(QUALITAET_ELBL1(k))+
     &abs(QUALITAET_BLBL(k))+
     &abs(QUALITAET_ELBL2(k)) 

        if(SUMME_QUALITAET(k).le.(EPOTVALUE(0,1)*0.02d0)) then          
                goto 999
        else
        continue
        end if

               


     
             


C       Update-Algorithmus
       
        
C       1. Kontaktleitwert Elektrode-Blech
        if(QUALITAET_ELBL1(k).ge.0.d0) then
          print*, 'Kontaktleitwert senken (ELBL1)'
        else if(QUALITAET_ELBL1(k).le.0.d0) then
          print*, 'Kontaktleitwert erhöhen (ELBL1)'
        end if       



C       Strom durch Übergang Elektrode-Blech          
          Gesamtstrom=STROM(10)*CurrentArea
C       Leitwertermittlung des Übergangs Elektrode-Blech

C       U=R*I=I/G --> G=I/U

        if(k.eq.1) then
        print*, Gesamtstrom, Qualitaet_ELBL1(k)        
        LEITWERT_ELBL1(k)=Gesamtstrom/QUALITAET_ELBL1(k)
        LEITWERT_ELBL1_SPEZ(k)=LEITWERT_ELBL1(k)/KONTAKTFLAECHE(1)
        print*, Leitwert_ELBL1(k), Kontaktflaeche(1)
        LEITWERT_BLBL(k)=Gesamtstrom/QUALITAET_BLBL(k)
        LEITWERT_BLBL_SPEZ(k)=LEITWERT_BLBL(k)/KONTAKTFLAECHE(2)
        LEITWERT_ELBL2(k)=Gesamtstrom/QUALITAET_ELBL2(k)
        LEITWERT_ELBL2_SPEZ(k)=LEITWERT_ELBL2(k)/KONTAKTFLAECHE(3)
        else

C        Differenzenbildung Anteilsbildung
C       Wieviel muss der Leitwert geändert werden??????

        LEITWERT_ELBL1(k)=LEITWERT_ELBL1(k-1)*((1.0d0)*
     &EPOTDIFFERENZ_ELBL1(k)/EPOTDIFFERENZ_ELBL1_VORGABE)
        
        
        LEITWERT_ELBL1_SPEZ(k)=LEITWERT_ELBL1(k)/KONTAKTFLAECHE(1)


        LEITWERT_BLBL(k)=LEITWERT_BLBL(k-1)*((1.0d0)*
     &EPOTDIFFERENZ_BLBL(k)/EPOTDIFFERENZ_BLBL_VORGABE)

        LEITWERT_BLBL_SPEZ(k)=LEITWERT_BLBL(k)/KONTAKTFLAECHE(2)

        LEITWERT_ELBL2(k)=LEITWERT_ELBL2(k-1)*((1.0d0)*
     &EPOTDIFFERENZ_ELBL2(k)/EPOTDIFFERENZ_ELBL2_VORGABE)
        
        
        LEITWERT_ELBL2_SPEZ(k)=LEITWERT_ELBL2(k)/KONTAKTFLAECHE(3)
        end if


        print*, LEITWERT_ELBL1_SPEZ(k)
        print*, LEITWERT_BLBL_SPEZ(k)
        print*, LEITWERT_ELBL2_SPEZ(k)
c        PAUSE

C      Übertrag in neue Iterationsschleife
      
        end do !k=1,10
        
        

999     continue  

       ANZAHLITERATIONEN=k-1      

       IterationElekNameZus='_WPS_ELEK_Iteration_ZUS'
      IterationElekFileNameZus=trim(Datum)//trim(IterationElekNameZus)
       print*, IterationElekFileNameZus
       IterationElekFileNameZus=adjustl(IterationElekFileNameZus)
       IterationElekFileNameZusFull=trim(IterationElekFileNameZus)//
     &trim(StringEndungTXT)
       print*, IterationElekFileNameZusFull

5       FORMAT(I6,4(',',1PE14.6))       

       open(77, file=trim(IterationElekFileNameZusFull),
     &STATUS='UNKNOWN')  
       do kk=0,50 
       write(77,5)kk,EPOTVALUE(kk,1), EPOTVALUE(kk,2),EPOTVALUE(kk,3),
     &EPOTVALUE(kk,4)
c       write(77,5)kk,LEITWERT_ELBL1_SPEZ(kk),LEITWERT_BLBL_SPEZ(kk),
c     &LEITWERT_ELBL2_SPEZ(kk) 
       end do
       close(77)             

       print*, EPOTVALUE
c       pause      

C**********Ende Erweiterung Iterationsschleife************************

        ELEKNUMBER=300
        ELEK='_ELEK_START'
        STRINGENDUNGINP='.inp'
        ELEKNAME=trim(Datum)//trim(ELEK)
        ELEKnameFULL=trim(Datum)//trim(ELEK)//trim(StringEndungInp)
        print*, ELEKnameFULL
        
C     *****************************************************************
C     ***  Erstellen der Abaqus-Input-Datei (elektrisch-thermisch) *** 
C     ***  ELEK_START ***       
C     *****************************************************************        

        open(ELEKNUMBER,file=trim(ELEKnameFULL), STATUS='UNKNOWN')
        write(ELEKNUMBER,*)'*Heading'
        write(ELEKNUMBER,*)'*Preprint, echo=NO, model=NO, history=NO
     &, contact=YES'
        write(ELEKNUMBER,*)'*Heading'
c        write(ELEKNUMBER,*)'*Part, name=Part-1'
        write(ELEKNUMBER,*)'*Node, nset=NALL'
C     *****************************************************************
C     ***  Einlesen des deformierten Netzes aus der durchgeführten 
C          mechanischen Berechnung ***        
C     *****************************************************************         
        write(ELEKNUMBER,*)'*INCLUDE, input=',trim(MechName),
     &'.015'
c     *****************************************************************
C     ***  Elementdefiniton - Vorgabe ***
c     *****************************************************************     
        write(ELEKNUMBER,*)'*INCLUDE, input=WPS_ELEK_ELEMENTE.txt'
c       if(IntegerRestartMechNumber.le.19) then 
c       JouleFraction=(IntegerRestartMechNumber+1)*0.05      
c       write(ELEKNUMBER,*)'*JOULE HEAT FRACTION'
c       write(ELEKNUMBER,*)JouleFraction
c       end if
       write(ELEKNUMBER,*)'**'
c     *****************************************************************
C     ***  Kontakt- und Materialdefinition ***          
       write(ELEKNUMBER,*)'*INCLUDE, input=WPS_ELEK_Contact.txt'
c     *****************************************************************
C     ***  Randbedingungen *** 
c     *****************************************************************        
       write(ELEKNUMBER,*)'*Initial Conditions, type=TEMPERATURE'
       write(ELEKNUMBER,*)'NALL, 20.'
       write(ELEKNUMBER,*)'*STEP,INC=10000'
       write(ELEKNUMBER,*)'*COUPLED THERMAL-ELECTRICAL, deltmx=5000'
       write(ELEKNUMBER,*)'0.0001, 0.001, , ,0.001'
       write(ELEKNUMBER,*)'*FIELD,variable=1,USER'
       write(ELEKNUMBER,*)'NALL'
       write(ELEKNUMBER,*)'*BOUNDARY'
       write(ELEKNUMBER,*)'BC_Elektrode, 9, , 0.0'
       write(ELEKNUMBER,*)'LOAD_ELEKTRODE, 11, ,20'
       write(ELEKNUMBER,*)'LOAD_ELEKTRODE_REFERENZ, 11, ,20'
       write(ELEKNUMBER,*)'BC_Elektrode, 11, , 20'
       write(ELEKNUMBER,*)'BC_Temp, 11, ,20'
       write(ELEKNUMBER,*)'*Dsecurrent'
       write(ELEKNUMBER,*)'SURF_LOAD_CURRENT,CS,', Strom(1)
       write(ELEKNUMBER,*)'*OUTPUT, FIELD'
       write(ELEKNUMBER,*)'*ELEMENT OUTPUT'
       write(ELEKNUMBER,*)'HFL,ECD,EPG,JENER'!, FV'
       write(ELEKNUMBER,*)'*NODE OUTPUT'
       write(ELEKNUMBER,*)'NT,EPOT'
C     ******** FIL-Datei zur Ermittlung des elektrischen Potentials****         
       write(ELEKNUMBER,*)'*NODE FILE'
       write(ELEKNUMBER,*)'EPOT'
       write(ELEKNUMBER,*)'*OUTPUT,HISTORY'
       write(ELEKNUMBER,*)'*ENERGY OUTPUT, VARIABLE=PRESELECT'
       write(ELEKNUMBER,*)'*END STEP'
       close(ELEKNUMBER)

C     *****************************************************************
C     ***  Subroutine zur Definiton des elektrischen Kontaktwiderstands
C     ***  Schnittstelle zu Kontaktdruck und Kontaktfläche zur möglichen
C     Einbindung von kontaktdruckabhängigen Formulierungen       
C     *****************************************************************       

       RestartElekNameFIELD='_WPS_FIELD'
       RestartElekFileNameFIELD=trim(Datum)//
     &trim(RestartElekNameFIELD)
       print*, RestartElekFileNameFIELD
       RestartElekFileNameFIELD=adjustl(RestartElekFileNameFIELD)
        RestartElekFileNameFullFIELD=trim(RestartElekFileNameFIELD)//
     &trim(StringEndungFor)
       print*, RestartElekFileNameFullFIELD

       print*,k-1, LEITWERT_ELBL1_SPEZ(k-1), LEITWERT_BLBL_SPEZ(k-1)

       open(ELEKNUMBER+1, file=trim(RestartELEKFileNameFullFIELD),
     &STATUS='UNKNOWN')

      write(ELEKNUMBER+1,*)'     module global'
      write(ELEKNUMBER+1,*)'     DOUBLE PRECISION ARRAY(1:',Knoten,'),
     & CAREA(1:3)'
      write(ELEKNUMBER+1,*)'     INTEGER SURFACE(1:3)'
      write(ELEKNUMBER+1,*)'     SAVE ARRAY, CAREA,SURFACE'
      write(ELEKNUMBER+1,*)'     end module' 
      write(ELEKNUMBER+1,*)'     SUBROUTINE UFIELD(FIELD,KFIELD,NSECPT,
     &KSTEP,KINC,TIME,NODE,'
      write(ELEKNUMBER+1,*)'    1 COORDS,TEMP,DTEMP,NFIELD)'    
      write(ELEKNUMBER+1,*)'     use global' 
      write(ELEKNUMBER+1,*)"     INCLUDE 'ABA_PARAM.INC'" 
      write(ELEKNUMBER+1,*)'     DIMENSION FIELD(NSECPT,NFIELD), TIME(2)
     &, COORDS(3),' 
      write(ELEKNUMBER+1,*)'    1 TEMP(NSECPT), DTEMP(NSECPT)' 
      write(ELEKNUMBER+1,*)'     INTEGER Initflag,I,J' 
      write(ELEKNUMBER+1,*)'     CHARACTER INFILE*255' 

       write(ELEKNUMBER+1,*)'      CHARACTER INFILE2*255'
       write(ELEKNUMBER+1,*)'      CHARACTER*256 JOBNAME'
       write(ELEKNUMBER+1,*)'      CHARACTER xoutdir*255, xfname*80'
       write(ELEKNUMBER+1,*)'      CHARACTER dmkname*255, fout*255'

      
      write(ELEKNUMBER+1,*)'     SAVE Initflag,KNOTEN,XCOORD,YCOORD' 
      write(ELEKNUMBER+1,*)'     INTEGER KNOTEN(1:',Knoten,')'
      write(ELEKNUMBER+1,*)'     DOUBLE PRECISION XCOORD(1:',Knoten,'),
     & YCOORD(1:',Knoten,')' 
      write(ELEKNUMBER+1,*)'      call GETJOBNAME( xfname, lxfname )'
      write(ELEKNUMBER+1,*)'      call GETOUTDIR( xoutdir,lxoutdir)'
      write(ELEKNUMBER+1,*)"      fout=dmkname(xfname(1:lxfname),xoutdir
     &(1:lxoutdir),'.out')" 

C********************************
C********************************
      write(ELEKNUMBER+1,*)"      INFILE=trim(fout)//'/",
     &trim(MECHNAME),"'//'.016'"
      write(ELEKNUMBER+1,*)"      INFILE2=trim(fout)//'/",
     &trim(MECHNAME),"'//'.017'"
C********************************
      write(ELEKNUMBER+1,*)'     if(Initflag.eq.0) then' 
      write(ELEKNUMBER+1,*)"     OPEN(UNIT=16,FILE=INFILE,
     &STATUS='UNKNOWN')" 
      write(ELEKNUMBER+1,*)'     Do I=1,',Knoten,'' 
      write(ELEKNUMBER+1,*)'     read(16,*) KNOTEN(I), XCOORD(I),
     & YCOORD(I), ARRAY(I)' 
      write(ELEKNUMBER+1,*)'     end do' 
      write(ELEKNUMBER+1,*)"     OPEN(UNIT=17,FILE=INFILE2,
     &STATUS='UNKNOWN')" 
      write(ELEKNUMBER+1,*)'     DO J=1,3' 
      write(ELEKNUMBER+1,*)'     read(17,*)SURFACE(J), CAREA(J)' 
      write(ELEKNUMBER+1,*)'     END DO' 
      write(ELEKNUMBER+1,*)'     close(16)' 
      write(ELEKNUMBER+1,*)'     close(17)'
      write(ELEKNUMBER+1,*)'     Initflag=1' 
      write(ELEKNUMBER+1,*)'     END IF' 
      write(ELEKNUMBER+1,*)'     FIELD(1,1)=ARRAY(NODE)'
      write(ELEKNUMBER+1,*)'     RETURN'
      write(ELEKNUMBER+1,*)'     END'
      write(ELEKNUMBER+1,*)'     SUBROUTINE GAPELECTR(SIGMA,D,TEMP,
     &PREDEF,TIME,CINAME,'
      write(ELEKNUMBER+1,*)'    1 SLNAME,MSNAME,COORDS,NODE,NPRED,KSTEP
     &,KINC)'   
      write(ELEKNUMBER+1,*)'     use global'
      write(ELEKNUMBER+1,*)"     INCLUDE 'ABA_PARAM.INC'"
      write(ELEKNUMBER+1,*)'     CHARACTER*80 CINAME,SLNAME,MSNAME'
      write(ELEKNUMBER+1,*)'     DIMENSION SIGMA(3),TEMP(2),PREDEF(2,*)
     &,TIME(2),COORDS(2,3)'
      write(ELEKNUMBER+1,*)'     If(ARRAY(NODE).gt.0.d0.OR.ARRAY(NODE)
     &.LT.0.d0) then'

      write(ELEKNUMBER+1,*)"     IF(CINAME.eq.'ELEKTRODE') then "
      write(ELEKNUMBER+1,*)'     SIGMA(1)=',LEITWERT_ELBL1_SPEZ(k-1)
      write(ELEKNUMBER+1,*)"     else if(CINAME.eq.'BLECH') then"
      write(ELEKNUMBER+1,*)'     SIGMA(1)=',LEITWERT_BLBL_SPEZ(k-1)
      write(ELEKNUMBER+1,*)'     END IF'
      write(ELEKNUMBER+1,*)'      ELSE'
      write(ELEKNUMBER+1,*)'     SIGMA(1)=0.d0'
      write(ELEKNUMBER+1,*)'     end if'      
      write(ELEKNUMBER+1,*)'     RETURN'
      write(ELEKNUMBER+1,*)'     END'
C******************************************************************************
       write(ELEKNUMBER+1,*)'      character*(*) FUNCTION dmkname
     &(fname,dname,exten)'
       write(ELEKNUMBER+1,*)'      character*(*) fname,dname,exten'
       write(ELEKNUMBER+1,*)'      ltot = len(fname)'
       write(ELEKNUMBER+1,*)'      lf = 0'
       write(ELEKNUMBER+1,*)'      DO k1 = ltot,2,-1'
       write(ELEKNUMBER+1,*)"       if (lf.EQ.0.AND.fname(k1:k1).NE.' ')
     & lf = k1"
       write(ELEKNUMBER+1,*)'      END DO'
       write(ELEKNUMBER+1,*)'      ltot = len(dname)'
       write(ELEKNUMBER+1,*)'      ld = 0'
       write(ELEKNUMBER+1,*)'      DO k1 = ltot,2,-1'
       write(ELEKNUMBER+1,*)"       IF (ld.EQ.0.AND.dname(k1:k1).NE.' ')
     &  ld = k1"
       write(ELEKNUMBER+1,*)'      END DO'
       write(ELEKNUMBER+1,*)'      ltot = len(exten)'
       write(ELEKNUMBER+1,*)'      le = 0'
       write(ELEKNUMBER+1,*)'      DO k1 = ltot,2,-1'
       write(ELEKNUMBER+1,*)"       IF (le.EQ.0.AND.exten(k1:k1).NE.' ')
     &  le = k1"
       write(ELEKNUMBER+1,*)'      END DO'
       write(ELEKNUMBER+1,*)'      IF ((lf+ld+le).LE.len(dmkname)) THEN'
c       write(ELEKNUMBER+1,*)"      dmkname=dname(1:ld)//'/'//
c     &fname(1:lf)"
       write(ELEKNUMBER+1,*)"      dmkname=dname(1:ld)"
       write(ELEKNUMBER+1,*)'      ltot = ld + lf + 1'
       write(ELEKNUMBER+1,*)'      IF ( le.GT.0) THEN'
c       write(ELEKNUMBER+1,*)'      dmkname = dmkname(1:ltot)//
c     &exten(1:le)'
       write(ELEKNUMBER+1,*)'      dmkname = dmkname(1:ltot)'
       write(ELEKNUMBER+1,*)'      END IF'
       write(ELEKNUMBER+1,*)'      END IF'
       write(ELEKNUMBER+1,*)'      RETURN'
       write(ELEKNUMBER+1,*)'      END'



C******************************************************************************      
C     *****************************************************************
C     *** Aufruf elektrisch-thermische Rechnung ***      
C     *****************************************************************

      AufrufElektrischeRechnung='abq2018 cpus=2  inter job='
     &//trim(ELEKName)//' user='//
     &trim(RestartElekFileNameFIELD)//' scratch=.'

       RestartElekFileNameOld=trim(RestartElekfileName)
       print*, AufrufElektrischeRechnung
     
      call system(AufrufElektrischeRechnung)

C     *****************************************************************
C     ***  Erstellung FPERT-Programm zum Auslesen der erstellten
C     Fil-Datei aus der elektrisch-thermischen Berechnung ***
C        *.015-Datei: Knotenkoordinaten des deformierten Netzes und
C        zugehöriges elektrisches Potential
C     *****************************************************************

       FpertName='_WPS_FPERT_EPOT'
       FpertFileName=trim(Datum)//trim(FPERTName)
       FPERTNUMBER=FPERTNUMBER+1
       print*, FPERTFILENAME
       STRINGENDUNGFOR='.for'
       FpertFileNameFULL=trim(FpertFileName)//StringEndungFor

        open(FPERTNUMBER, file=trim(FPERTFileNameFull),
     &STATUS='UNKNOWN') 
       write(FPERTNUMBER,*)'     SUBROUTINE ABQMAIN'
       write(FPERTNUMBER,*)"     INCLUDE 'aba_param.inc'"
       write(FPERTNUMBER,*)'     DIMENSION  ARRAY(513), JRRAY
     &(NPRECD,513),LRUNIT(2,1)'  
       write(FPERTNUMBER,*)'     EQUIVALENCE (ARRAY(1), JRRAY(1,1))'
       write(FPERTNUMBER,*)'     PARAMETER (ITOTAL = ',Knoten,')'
       write(FPERTNUMBER,*)'     DIMENSION DISP(6,ITOTAL),
     & COORD(3,ITOTAL)'
       write(FPERTNUMBER,*)'     DIMENSION EPOT(ITOTAL)'
       write(FPERTNUMBER,*)'     DOUBLE PRECISION CAREA(1:10)'
       write(FPERTNUMBER,*)'     DIMENSION INODE(ITOTAL), IDOF(30),
     & JEIGNO(10)'
       write(FPERTNUMBER,*)'     DOUBLE PRECISION DISPLACEMENT
     &(1:',Knoten,',1:2)'
       write(FPERTNUMBER,*)'     CHARACTER FNAME*80,OUTFILE*(*)'
       write(FPERTNUMBER,*)"     PARAMETER (OUTFILE = '",
     &trim(ELEKName),".015')"
       write(FPERTNUMBER,*)'     ICYCLE = 0'
       write(FPERTNUMBER,*)'     I1901  = 0'
       write(FPERTNUMBER,*)'     I101   = 0'
       write(FPERTNUMBER,*)'     I      = 1'
       write(FPERTNUMBER,*)'     K      = 1'
       write(FPERTNUMBER,*)'     J      = 0'
       write(FPERTNUMBER,*)'     NRU = 1'             
       write(FPERTNUMBER,*)'     LRUNIT(1,NRU) = 8'
       write(FPERTNUMBER,*)'     LRUNIT(2,NRU) = 2'
       write(FPERTNUMBER,*)'     LOUTF = 0'
       write(FPERTNUMBER,*)"     OPEN(UNIT=15,FILE=OUTFILE,STATUS=
     &'UNKNOWN',IOSTAT = J)"
       write(FPERTNUMBER,*)'     IF (J .NE. 0) THEN'
       write(FPERTNUMBER,*)'      WRITE(*,*) OUTFILE'
c       write(FPERTNUMBER,*)'      GOTO 950'
       write(FPERTNUMBER,*)'     END IF'      
       write(FPERTNUMBER,*)"     FNAME='",trim(ElekName),"'"
       write(FPERTNUMBER,*)'     CALL  INITPF
     & (FNAME, NRU, LRUNIT, LOUTF)'
       write(FPERTNUMBER,*)'     JUNIT = LRUNIT(1,NRU)'
       write(FPERTNUMBER,*)'     CALL  DBRNU (JUNIT)'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE (JRCD .EQ. 0)'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,2) .EQ. 1921 ) THEN'
       write(FPERTNUMBER,*)'     NODEMAX = JRRAY(1,8)'
       write(FPERTNUMBER,*)'     IELMAX  = JRRAY(1,7)'
       write(FPERTNUMBER,*)'     ICYCLE = ICYCLE +1'
       write(FPERTNUMBER,*)'     ENDIF'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     ENDDO'
       write(FPERTNUMBER,*)'     icycle=1'
       write(FPERTNUMBER,*)'     CALL DBFILE (2, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     WRITE(*,*) NODEMAX, IELMAX'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE (JRCD .EQ. 0)'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,2) .EQ. 1901 .AND.
     & ICYCLE .LE. 1) THEN'
       write(FPERTNUMBER,*)'     I1901 = I1901 + 1'
       write(FPERTNUMBER,*)'     INODE(I1901)  = JRRAY(1,3)'
       write(FPERTNUMBER,*)'     COORD(1,I1901) = ARRAY(4)'
       write(FPERTNUMBER,*)'     COORD(2,I1901) = ARRAY(5)'
       write(FPERTNUMBER,*)'     COORD(3,I1901) = 0.0D0'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,1) .GE. 6) COORD(3,I1901)
     & = ARRAY(6)'
       write(FPERTNUMBER,*)'     ELSE IF (JRRAY(1,2) .EQ. 1902) THEN'
       write(FPERTNUMBER,*)'     DO 15 IXX = 1, JRRAY(1,1)-2'
       write(FPERTNUMBER,*)'     IDOF(IXX) = JRRAY(1,IXX+2)'
       write(FPERTNUMBER,*)'15   CONTINUE'
       write(FPERTNUMBER,*)'     ITRANS = 3'
       write(FPERTNUMBER,*)'     IF (IDOF(3) .EQ. 0) ITRANS = 2'
       write(FPERTNUMBER,*)'     end if'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     END DO'
       write(FPERTNUMBER,*)'     CALL DBFILE (2, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     icycle=1'
       write(FPERTNUMBER,*)'     call posfil(1,6,ARRAY,JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE(JRCD.eq.0)'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,2) .EQ. 105.and.
     &icycle.le.1) THEN'  
       write(FPERTNUMBER,*)'     I105 = JRRAY(1,3)'
       write(FPERTNUMBER,*)'     EPOT(I105)=ARRAY(4)'
       write(FPERTNUMBER,*)'     END IF'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     END DO'
       write(FPERTNUMBER,*)'     WRITE(*,100) OUTFILE'
       write(FPERTNUMBER,*)"100  FORMAT(//,2X,'The perturbed mesh
     & data are being written to:',"
       write(FPERTNUMBER,*)'    &1X,A,//)'
       write(FPERTNUMBER,*)'     DO K = 1, NODEMAX' 
       write(FPERTNUMBER,*)'     WRITE(15,110) INODE(K), (COORD(J,K),J
     & = 1, 3),'
       write(FPERTNUMBER,*)'    1 EPOT(INODE(K))'
       write(FPERTNUMBER,*)"110  FORMAT(I6,4(',',1PE14.6))"    
       write(FPERTNUMBER,*)'     ENDDO' 
       write(FPERTNUMBER,*)'     CLOSE (15)'
       write(FPERTNUMBER,*)'     WRITE(*,120) ' 
       write(FPERTNUMBER,*)"120  FORMAT(//,2X,' . . . PROGRAM FINISHED
     & SUCCESSFULLY . . . ')"
       write(FPERTNUMBER,*)'     RETURN'
       write(FPERTNUMBER,*)'     END'
       close(FPERTNUMBER)

C     *****************************************************************
C     *** Aufruf Auslesen der Fil-Datei (elektrisch-thermisch) ***      
C     *****************************************************************
       AufrufFPERT='abq2018 make job=' // trim(FPERTFILENAME)

       call system(AufrufFpert) 

       AufrufFPERT2='abq2018 ' // trim(FPERTFILENAME)

       call system(AufrufFpert2) 


c        fpertmake2='abq2018 make job=100127_WPS_FPERT_EPOT'
c        fpert2='abq2018 100127_WPS_FPERT_EPOT'

c        pause

         FILENUMBER=10001
         FPERTNUMBER=20001
         ELEKNUMBER=30001
         STEP=3
         LAUF=1
         IntegerRestartMechNumber=1
         IntegerRestartSTEPNumber=1


C     *****************************************************************
C     *** Beginn Zeitschleife / Restarts ***      
C     *****************************************************************

         do Inkrement=1,800 !600ms Schweißzeit + 500ms Nachhaltezeit + 7,5s Abkühlung
        
C**************Uebergang IntegerString************************        
        write(a,*)IntegerRestartMechnumber        
        write(b,*)IntegerRestartSTEPnumber

C**************************************************************        
c        StringRestartMechNumber='1'
        
        
c        StringMechStepNameNumber='1'
        a=adjustl(a)
        b=adjustl(b)

        StringRestartMechNumber=a
        StringMechSTEPNameNumber=b

        
        MechStepName='MECHSTEP-'
        RestartMechName='_WPS_MECH_RESTART'
        RestartMechFileName=trim(Datum)//trim(RestartMechName)//
     &trim(StringRestartMechNumber)
        print*, RestartMechFileName

c        pause

       

        RestartMechFileNameFull=trim(RestartMechFileName)//
     &trim(StringEndungInp)

        print*, RestartMechFileNameFull
        MechStepNameFull=trim(MechStepName)//
     &trim(StringMechStepNameNumber)
c        pause

C     *****************************************************************
C     *** Zum korrekten Übertrag des Temperaturfeldes wird die
C     Inkrementanzahl der elektrisch-thermischen Berechnung ausgelesen***      
C     *****************************************************************

        if(lauf.gt.1) then
        open(ELEKNUMBER+1,file=trim(Restartelekfilename)//'.out',
     &STATUS='UNKNOWN')
        read(ELEKNUMBER+1,*) ELEK1INK
        print*, 'ElekInk:', ELEK1INK
        close(ELEKNUMBER+1)
        end if
        write(c,*)ELEK1INK
        c=adjustl(c)

C     *****************************************************************
C     ***  Erstellen der Abaqus-Input-Datei (mechanisch) / Restart ***        
C     *****************************************************************  
        
       open(FILENUMBER, file=trim(RestartMechFileNameFull),
     &STATUS='UNKNOWN') 

       write(FILENUMBER,*)'*restart,read,step=',STEP,',write, overlay'
C     *****************************************************************
C     ***  Schrittdefinition *** 
C     *****************************************************************          
       write(FILENUMBER,*)'*Step,
     & name=',trim(MECHSTEPNAMEFULL),',nlgeom=yes, inc=100000'
       write(FILENUMBER,*)'*STATIC'
C     *****************************************************************
C     ***  Dynamische Inkrementierung, um die Berechnungszeit zu
C     reduzieren / Modellspezifisch zu konzipieren ***        
C     ***************************************************************** 
       if(LAUF.gt.650)then  
       write(FILENUMBER,*)'0.0125, 0.05,, 0.0125'
       else if(lauf.gt.600.and.lauf.le.650) then             
       write(FILENUMBER,*)'0.0025, 0.01,, 0.0025'
       else               
       write(FILENUMBER,*)'0.00025, 0.001, 1e-20, 0.00025'
       end if
	   write(FILENUMBER,*)'**' 
       write(FILENUMBER,*)'** CONTROLS'
       write(FILENUMBER,*)'**' 
       write(FILENUMBER,*)'*Controls, reset'
       write(FILENUMBER,*)'*Controls, parameters=time incrementation'
       write(FILENUMBER,*)', , , , , , , 100, , ,' 
       write(FILENUMBER,*)'**' 
C     *****************************************************************
C     ***  Randbedingungen / Abheben der Elektrode ***        
C     *****************************************************************
       write(FILENUMBER,*)'*Boundary, OP=NEW'
       IF(LAUF.EQ.651) then
       write(FILENUMBER,*)'NSET_E1, 2,2, 0.5'
       write(FILENUMBER,*)'NSET_E2, 2,2, -0.5'
       write(FILENUMBER,*)'*Boundary,fixed, OP=NEW'
       write(FILENUMBER,*)'BC_Blechfix, 2,2,0'
       write(FILENUMBER,*)'**'
       write(FILENUMBER,*)'*MODEL CHANGE, type=CONTACT, remove'
c       write(FILENUMBER,*)'SURF_BL1_EL1, SURF_EL1_BL1'
c       write(FILENUMBER,*)'SURF_BL2_EL2, SURF_EL2_BL2'
       write(FILENUMBER,*)'SURF2_BL1_EL1, SURF2_EL1_BL1' 
       write(FILENUMBER,*)'SURF2_BL2_EL2, SURF2_EL2_BL2'
       else if(LAUF.EQ.652) then
       write(FILENUMBER,*)'NSET_E1, 2,2, 0.5'
       write(FILENUMBER,*)'NSET_E2, 2,2, -0.5'
       write(FILENUMBER,*)'*Boundary,fixed, OP=NEW'
       write(FILENUMBER,*)'BC_Blechfix, 2,2,0'
       else if(LAUF.GT.652) then
       write(FILENUMBER,*)'*Boundary,fixed, OP=NEW'
       write(FILENUMBER,*)'BC_Blechfix, 2,2,0'
       write(FILENUMBER,*)'NSET_E1, 2,2, 0.5'
       write(FILENUMBER,*)'NSET_E2, 2,2, -0.5'
       ELSE
       write(FILENUMBER,*)'BC_Elektrode, ENCASTRE'
       end if 

       if(LAUF.eq.1)then
       write(FILENUMBER,*)'*TEMPERATURE, BSTEP=1,BINC=1, ESTEP=1,
     &EINC=6,file=',trim(ELEKNAME),'.odb'
C     *****************************************************************
C     ***  Kontaktpaarwechsel SEP/NOSEP ***        
C     ****************************************************************            
       else if(LAUF.EQ.601) then
       write(FILENUMBER,*)'*MODEL CHANGE, type=Contact Pair, remove'
       write(FILENUMBER,*)'SURF_BL2_BL1, SURF_BL1_BL2'
       write(FILENUMBER,*)'*MODEL CHANGE, type=Contact Pair, add'
       write(FILENUMBER,*)'SURF2_BL2_BL1, SURF2_BL1_BL2'
       write(FILENUMBER,*)'*MODEL CHANGE, type=Contact Pair, remove'
       write(FILENUMBER,*)'SURF_BL1_EL1, SURF_EL1_BL1'
       write(FILENUMBER,*)'*MODEL CHANGE, type=Contact Pair, add'
       write(FILENUMBER,*)'SURF2_BL1_EL1, SURF2_EL1_BL1' 
       write(FILENUMBER,*)'*MODEL CHANGE, type=Contact Pair, remove'
       write(FILENUMBER,*)'SURF_BL2_EL2, SURF_EL2_BL2'
       write(FILENUMBER,*)'*MODEL CHANGE, type=Contact Pair, add'
       write(FILENUMBER,*)'SURF2_BL2_EL2, SURF2_EL2_BL2'  
       write(FILENUMBER,*)'*TEMPERATURE, BSTEP=1,BINC=1, ESTEP=1,
     &EINC=',trim(c),',file=',trim(RESTARTELEKFILENAME),'.odb' 
       else
       write(FILENUMBER,*)'*TEMPERATURE, BSTEP=1,BINC=1, ESTEP=1,
     &EINC=',trim(c),',file=',trim(RESTARTELEKFILENAME),'.odb'
       end if  
C     *****************************************************************
C     ***  Dynamische Inkrementierung, um die Berechnungszeit zu
C     reduzieren / Modellspezifisch zu konzpieren ***
C     *** Ermittlung der Elektrodenklemmkraft ****       
C     *****************************************************************              
       write(FILENUMBER,*)'*CLOAD'
       if(INKREMENT.LE.600) then
       write(FILENUMBER,*)'LOAD_ELEKTRODE_REFERENZ,
     &2,',Kraft(Inkrement)
       else if (LAUF.GT.600.AND.LAUF.LE.650) then
       write(FILENUMBER,*)'LOAD_ELEKTRODE_REFERENZ,
     &2,',Kraft(10*(Inkrement-600)+600)
       else
       write(FILENUMBER,*)'LOAD_ELEKTRODE_REFERENZ,2,0.d0'
       end if       
       write(FILENUMBER,*)'**OUTPUT REQUESTS'
       write(FILENUMBER,*)'*Restart, write, frequency=1, overlay'
       write(FILENUMBER,*)'**'
C     *****************************************************************
C     *** Fil-File Definiton ***      
C     *****************************************************************         
       write(FILENUMBER,*)'*NODE FILE'
       write(FILENUMBER,*)'U'
       write(FILENUMBER,*)'*CONTACT FILE'
       write(FILENUMBER,*)'CAREA'
       write(FILENUMBER,*)'*CONTACT FILE, slave=SURF_BL2_BL1'
       write(FILENUMBER,*)'*CONTACT FILE, slave=SURF_BL1_EL1'
       write(FILENUMBER,*)'*CONTACT FILE, slave=SURF_BL2_EL2'
       write(FILENUMBER,*)'*CONTACT FILE, slave=SURF2_BL2_BL1'
       write(FILENUMBER,*)'*CONTACT FILE, slave=SURF2_BL1_EL1'
       write(FILENUMBER,*)'*CONTACT FILE, slave=SURF2_BL2_EL2'
       write(FILENUMBER,*)'*Output, field, variable=PRESELECT'
       write(FILENUMBER,*)'*NODE OUTPUT'
       write(FILENUMBER,*)'COORD, NT'
       write(FILENUMBER,*)'**'
C     *****************************************************************
C     *** ODB-Definitionen ***      
C     *****************************************************************        
       write(FILENUMBER,*)'*ELEMENT OUTPUT'
       write(FILENUMBER,*)'TEMP'
c       write(FILENUMBER,*)'*Element Output, directions=YES'
c       write(FILENUMBER,*)'FV, SDV'
       write(FILENUMBER,*)'*NODE PRINT, nset=NALL'
       write(FILENUMBER,*)'COORD'
       write(FILENUMBER,*)'*Output, history, variable=PRESELECT'        
       write(FILENUMBER,*)'*CONTACT OUTPUT'
       write(FILENUMBER,*)'CAREA'
       write(FILENUMBER,*)'*End Step'
       close(FILENUMBER)
C     *****************************************************************
C     *** Aufruf mechanische Rechnung / Restart ***      
C     *****************************************************************

       if(lauf.eq.1) then
       AufrufMechanischeRechnung='abq2018 cpus=2  inter job='
     &//trim(RESTARTMECHFILENAME)//' oldjob='//trim(MECHNAME)//
     &' user='//trim(UVARMNAME)//' scratch=.'      
       else
      AufrufMechanischeRechnung='abq2018 cpus=2  inter job='
     &//trim(RESTARTMECHFILENAME)//'
     & oldjob='//trim(RestartMechFileNameOld)//' user='//trim(UVARMNAME)
     &//' scratch=.'  
       end if
       print*, AufrufMechanischeRechnung
       RestartMechFileNameOld=trim(RestartMechfileName)
     
       call system(AufrufMechanischeRechnung)


C     *****************************************************************
C     ***  Erstellung FPERT-Programm zum Auslesen der erstellten
C     Fil-Datei aus der mechanischen Berechnung ***
C        *.015-Datei: Knotenkoordinaten des deformierten Netzes
C        *.016-Datei: Knotenkoordinaten und zugehöriger Kontaktdruck
C        *.017-Datei: Kontaktfläche CAREA
C     Out-Datei aus der mechanischen Berechnung wird benötigt zur Definition
C     des auszulesenden Steps und Inkrement (Hinweis: Fil-File hinterlegt
C     auch bei Restart alle vorangegangene Ergebnisse)
C     *****************************************************************
       FpertName='_WPS_FPERT_CAREA'
       FpertFileName=trim(Datum)//trim(FPERTName)//
     &trim(StringRestartMechNumber)
       print*, FPERTFILENAME
       FpertFileNameFULL=trim(FpertFileName)//StringEndungFor

        open(FPERTNUMBER, file=trim(FPERTFileNameFull),
     &STATUS='UNKNOWN') 
        open(FPERTNUMBER+1,file=trim(RESTARTMECHFILENAME)//'.out',
     &STATUS='UNKNOWN')
        read(FPERTNUMBER+1,*) MECHINK
        print*, 'MECHINK:', MECHINK
        close(FPERTNUMBER+1)

       write(FPERTNUMBER,*)'     SUBROUTINE ABQMAIN'
       write(FPERTNUMBER,*)"     INCLUDE 'aba_param.inc'"
       write(FPERTNUMBER,*)'     DIMENSION  ARRAY(513), JRRAY
     &(NPRECD,513),LRUNIT(2,1)'  
       write(FPERTNUMBER,*)'     EQUIVALENCE (ARRAY(1), JRRAY(1,1))'
       write(FPERTNUMBER,*)'     PARAMETER (ITOTAL = ',Knoten,')'
       write(FPERTNUMBER,*)'     DIMENSION DISP(6,ITOTAL),
     & COORD(3,ITOTAL)'
       write(FPERTNUMBER,*)'     DIMENSION CPRESS(ITOTAL)'
       write(FPERTNUMBER,*)'     DOUBLE PRECISION CAREA(1:10)'
       write(FPERTNUMBER,*)'     DIMENSION INODE(ITOTAL), IDOF(30),
     & JEIGNO(10)'
       write(FPERTNUMBER,*)'     DOUBLE PRECISION DISPLACEMENT
     &(1:',Knoten,',1:2)'
       write(FPERTNUMBER,*)'     CHARACTER FNAME*80,OUTFILE*(*)'
       write(FPERTNUMBER,*)'     CHARACTER OUTFILE2*(*)'
       write(FPERTNUMBER,*)'     CHARACTER OUTFILE3*(*)'
       write(FPERTNUMBER,*)"     PARAMETER (OUTFILE = '",
     &trim(RestartMechfileName),".015')"
       write(FPERTNUMBER,*)"     PARAMETER (OUTFILE2 = '",
     &trim(RestartMechfileName),".016')"
       write(FPERTNUMBER,*)"     PARAMETER (OUTFILE3 = '",
     &trim(RestartMechfileName),".017')"
       write(FPERTNUMBER,*)'     ICYCLE = 0'
       write(FPERTNUMBER,*)'     I1901  = 0'
       write(FPERTNUMBER,*)'     I101   = 0'
       write(FPERTNUMBER,*)'     I      = 1'
       write(FPERTNUMBER,*)'     K      = 1'
       write(FPERTNUMBER,*)'     J      = 0'
       write(FPERTNUMBER,*)'     NRU = 1'             
       write(FPERTNUMBER,*)'     LRUNIT(1,NRU) = 8'
       write(FPERTNUMBER,*)'     LRUNIT(2,NRU) = 2'
       write(FPERTNUMBER,*)'     LOUTF = 0'
       write(FPERTNUMBER,*)"     OPEN(UNIT=15,FILE=OUTFILE,STATUS=
     &'UNKNOWN',IOSTAT = J)"
       write(FPERTNUMBER,*)'     IF (J .NE. 0) THEN'
       write(FPERTNUMBER,*)'      WRITE(*,*) OUTFILE'
c       write(FPERTNUMBER,*)'      GOTO 950'
       write(FPERTNUMBER,*)'     END IF'
       write(FPERTNUMBER,*)"     OPEN(UNIT=16,FILE=OUTFILE2,STATUS=
     &'UNKNOWN',IOSTAT = J)"
       write(FPERTNUMBER,*)'     IF (J .NE. 0) THEN'
       write(FPERTNUMBER,*)'      WRITE(*,*) OUTFILE'
c       write(FPERTNUMBER,*)'      GOTO 950'
       write(FPERTNUMBER,*)'     END IF'
       write(FPERTNUMBER,*)"     OPEN(UNIT=17,FILE=OUTFILE3,STATUS=
     &'UNKNOWN',IOSTAT = J)"
       write(FPERTNUMBER,*)'     IF (J .NE. 0) THEN'
       write(FPERTNUMBER,*)'      WRITE(*,*) OUTFILE'
c       write(FPERTNUMBER,*)'      GOTO 950'
       write(FPERTNUMBER,*)'     END IF'
       write(FPERTNUMBER,*)"     FNAME='",trim(RestartMechfileName),"'"
       write(FPERTNUMBER,*)'     CALL  INITPF
     & (FNAME, NRU, LRUNIT, LOUTF)'
       write(FPERTNUMBER,*)'     JUNIT = LRUNIT(1,NRU)'
       write(FPERTNUMBER,*)'     CALL  DBRNU (JUNIT)'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE (JRCD .EQ. 0)'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,2) .EQ. 1921 ) THEN'
       write(FPERTNUMBER,*)'     NODEMAX = JRRAY(1,8)'
       write(FPERTNUMBER,*)'     IELMAX  = JRRAY(1,7)'
       write(FPERTNUMBER,*)'     ICYCLE = ICYCLE +1'
       write(FPERTNUMBER,*)'     ENDIF'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     ENDDO'
       write(FPERTNUMBER,*)'     icycle=1'
       write(FPERTNUMBER,*)'     CALL DBFILE (2, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     WRITE(*,*) NODEMAX, IELMAX'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE (JRCD .EQ. 0)'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,2) .EQ. 1901 .AND.
     & ICYCLE .LE. 1) THEN'
       write(FPERTNUMBER,*)'     I1901 = I1901 + 1'
       write(FPERTNUMBER,*)'     INODE(I1901)  = JRRAY(1,3)'
       write(FPERTNUMBER,*)'     COORD(1,I1901) = ARRAY(4)'
       write(FPERTNUMBER,*)'     COORD(2,I1901) = ARRAY(5)'
       write(FPERTNUMBER,*)'     COORD(3,I1901) = 0.0D0'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,1) .GE. 6) COORD(3,I1901)
     & = ARRAY(6)'
       write(FPERTNUMBER,*)'     ELSE IF (JRRAY(1,2) .EQ. 1902) THEN'
       write(FPERTNUMBER,*)'     DO 15 IXX = 1, JRRAY(1,1)-2'
       write(FPERTNUMBER,*)'     IDOF(IXX) = JRRAY(1,IXX+2)'
       write(FPERTNUMBER,*)'15   CONTINUE'
       write(FPERTNUMBER,*)'     ITRANS = 3'
       write(FPERTNUMBER,*)'     IF (IDOF(3) .EQ. 0) ITRANS = 2'
       write(FPERTNUMBER,*)'     end if'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     END DO'
       write(FPERTNUMBER,*)'     CALL DBFILE (2, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     icycle=1'
C      Änderung 01.11.09 --> Inkrementauswahl       
       write(FPERTNUMBER,*)'     call posfil(',(STEP+1),',',
     &(MECHINK), !27.01.09 Hier muss geprüft werden
     &',ARRAY,JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE(JRCD.eq.0)'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,2) .EQ. 101.and.
     &icycle.le.1) THEN'  
       write(FPERTNUMBER,*)'     I101 = I101 + 1'
       write(FPERTNUMBER,*)'     DISP(1,I101) = ARRAY(4)'
       write(FPERTNUMBER,*)'     DISP(2,I101) = ARRAY(5)'
       write(FPERTNUMBER,*)'     IF (INODE(I101) .EQ. 0) INODE(I101) =
     & JRRAY(1,3)'
       write(FPERTNUMBER,*)'     if(i101.eq.NODEMAX) then'
       write(FPERTNUMBER,*)'     ICYCLE=ICYCLE+1'
       write(FPERTNUMBER,*)'     CALL NODEGEN(COORD,DISP,I1901,FACTOR)'
       write(FPERTNUMBER,*)'     ENDIF '
       write(FPERTNUMBER,*)'     ENDIF'
       write(FPERTNUMBER,*)'     CALL DBFILE(0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     ENDDO'
!      27.01. Ausgabe von CAREA und CPRESS
       write(FPERTNUMBER,*)'     CALL DBFILE(2, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     icycle=1'
       write(FPERTNUMBER,*)'     CPRESS=0.d0'
       write(FPERTNUMBER,*)'     call posfil(',(STEP+1),',',
     &(MECHINK), !27.01.09 Hier muss geprüft werden
     &',ARRAY,JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE (JRCD .EQ. 0)'
       write(FPERTNUMBER,*)'     IF(JRRAY(1,2) .EQ. 1504 .AND. ICYCLE
     & .LE. 1) THEN'
       write(FPERTNUMBER,*)'     I1511=JRRAY(1,3)'
       write(FPERTNUMBER,*)'     ELSE IF(JRRAY(1,2) .EQ. 1511 .AND.
     & ICYCLE.LE. 1) THEN'
       write(FPERTNUMBER,*)'     CPRESS(I1511) = ARRAY(3)'
       write(FPERTNUMBER,*)'     end if'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     end do'
       write(FPERTNUMBER,*)'     CALL DBFILE(2, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     icycle=1'
       write(FPERTNUMBER,*)'     CAREA=0.d0'
       write(FPERTNUMBER,*)'     i=1'
       write(FPERTNUMBER,*)'     call posfil(',(STEP+1),',',
     &(MECHINK), !27.01.09 Hier muss geprüft werden
     &',ARRAY,JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE (JRCD .EQ. 0)'
       write(FPERTNUMBER,*)'     IF(JRRAY(1,2) .EQ. 1503 .AND. ICYCLE
     & .LE. 1) THEN'
       write(FPERTNUMBER,*)'     ELSE IF(JRRAY(1,2) .EQ. 1524 .AND.
     & ICYCLE .LE. 1) THEN'
       write(FPERTNUMBER,*)'     CAREA(i) = ARRAY(3)'
       write(FPERTNUMBER,*)'     i=i+1'
       write(FPERTNUMBER,*)'     END IF'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     end do'
       write(FPERTNUMBER,*)'     WRITE(*,100) OUTFILE'
       write(FPERTNUMBER,*)"100  FORMAT(//,2X,'The perturbed mesh
     & data are being written to:',"
       write(FPERTNUMBER,*)'    &1X,A,//)'
       write(FPERTNUMBER,*)'     DO K = 1, NODEMAX' 
       write(FPERTNUMBER,*)'     WRITE(15,110) INODE(K), (COORD(J,K),J
     & = 1, ITRANS)'
       write(FPERTNUMBER,*)'     WRITE(16,110) K, COORD(1,K),
     &COORD(2,K), CPRESS(K)'
       write(FPERTNUMBER,*)"110  FORMAT(I6,3(',',1PE14.6))"    
       write(FPERTNUMBER,*)'     ENDDO' 
       write(FPERTNUMBER,*)'     DO I=1,6'
       write(FPERTNUMBER,*)'     WRITE(17,*)i, CAREA(i)'
       write(FPERTNUMBER,*)'     ENDDO'
       write(FPERTNUMBER,*)'     CLOSE (15)'
       write(FPERTNUMBER,*)'     CLOSE (16)'
       write(FPERTNUMBER,*)'     CLOSE (17)' 
       write(FPERTNUMBER,*)'     WRITE(*,120) ' 
       write(FPERTNUMBER,*)"120  FORMAT(//,2X,' . . . PROGRAM FINISHED
     & SUCCESSFULLY . . . ')"
       write(FPERTNUMBER,*)'     RETURN'
       write(FPERTNUMBER,*)'     END'
       write(FPERTNUMBER,*)'     SUBROUTINE NODEGEN
     &(COORD,DISP,I1901,FACTOR)'
       write(FPERTNUMBER,*)"     INCLUDE 'aba_param.inc'"
       write(FPERTNUMBER,*)'     DIMENSION COORD(3,*),DISP(6,*)'
       write(FPERTNUMBER,*)'     DO I = 1, I1901'
       write(FPERTNUMBER,*)'     COORD(1,I) = COORD(1,I) +  DISP(1,I)'
c       write(FPERTNUMBER,*)'     print*, I, COORD(1,I), DISP(1,I)'
c       write(FPERTNUMBER,*)'     pause'
       write(FPERTNUMBER,*)'     if(COORD(1,I).lt.0.d0) then'
       write(FPERTNUMBER,*)'     COORD(1,I)=0.d0'
       write(FPERTNUMBER,*)'     end if'
       write(FPERTNUMBER,*)'     COORD(2,I) = COORD(2,I) +  DISP(2,I)'
       write(FPERTNUMBER,*)'     COORD(3,I) = COORD(3,I) +  DISP(3,I)'
       write(FPERTNUMBER,*)'     END DO'
       write(FPERTNUMBER,*)'     RETURN'
       write(FPERTNUMBER,*)'900  FORMAT(//,'
       write(FPERTNUMBER,*)"    &/,2X,'TROUBLE OPENING FILE',1X,A)"
       write(FPERTNUMBER,*)'950  WRITE(*,1000)'
       write(FPERTNUMBER,*)'1000 FORMAT(//,'
       write(FPERTNUMBER,*)"    &/,2X,' . . . TROUBLE READING DATA . 
     &. .   ',"
       write(FPERTNUMBER,*)"    &/,2X,' . . .   PROGRAM STOPPED    . .
     & .   ',/)"
       write(FPERTNUMBER,*)'2000 FORMAT(//,'
       write(FPERTNUMBER,*)"    & /,'   +-----------------+',"
       write(FPERTNUMBER,*)"    & /,'   +------- ---------+',//)"
       write(FPERTNUMBER,*)"2010 FORMAT(//,"       
       write(FPERTNUMBER,*)"    &/,2X,'Nodes. . . . . . . ',I5,"
       write(FPERTNUMBER,*)"    &/,2X,'Elements   . . . . . . . ',I5)"
       write(FPERTNUMBER,*)'     RETURN'
       write(FPERTNUMBER,*)'     END'
       close(FPERTNUMBER)

C     *****************************************************************
C     *** Aufruf Auslesen der Fil-Datei ***      
C     ***************************************************************** 
       AufrufFPERT='abq2018 make job=' // trim(FPERTFILENAME)

       call system(AufrufFpert) 

       AufrufFPERT2='abq2018 ' // trim(FPERTFILENAME)

       call system(AufrufFpert2)    

c       call system(AufrufFpert2)
       STRINGENDUNGFIL='.fil'
       DELNAME=trim(RestartMechFileName)//trim(StringEndungFil)
       DEL='del '// trim(DELNAME)  
       call system(DEL)

C********************************************************************
C********************** Iteration für Restart-Schritte***************
C*Vorgehensweise absolut identisch zu ELEK_START, da kein Restart der
Celektrischen Rechnung durchgeführt wird.****************************
C********************************************************************
C****** Iterative Bestimmung des spezifischen Kontaktleitwerts*******

        
        k=0
        STRINGENDUNGEPOT='.015'      
c        EPOTNAME=trim(ELEKNAME)//StringEndungEPOT
        EPOTVALUE=0.d0


C      Dies muss aus experimentellen Daten eingelesen werden

       EPOTVALUE(k,1)=EPOTVALUEEXPERIMENT((Inkrement+1),1)
       EPOTVALUE(k,2)=EPOTVALUEEXPERIMENT((Inkrement+1),2) 
       EPOTVALUE(k,3)=EPOTVALUEEXPERIMENT((Inkrement+1),3)
       EPOTVALUE(k,4)=EPOTVALUEEXPERIMENT((Inkrement+1),4)
C      *****************************************************       
        
       EPOTDIFFERENZ(k,1)=EPOTVALUE(k1,1)-EPOTVALUE(k,4)
       EPOTDIFFERENZ(k,2)=EPOTVALUE(k1,2)-EPOTVALUE(k,4)
       EPOTDIFFERENZ(k,3)=EPOTVALUE(k,3)-EPOTVALUE(k,4)

       print*, EPOTVALUE(k,1), EPOTVALUE(k,2),EPOTVALUE(k,3),
     &EPOTVALUE(k,4) 


C     Interne Schleife zur Bestimmung des Kontaktleitwerts

       

       IntegerIterationElekNumber=1
c       write(a,*)IntegerIterationElekNumber
c       a=adjustl(a)
c       StringIterationElekNumber=a

       ITNUMBER=40001
       FPERTNUMBIT=50001

C***********Kontaktflächen zugänglich machen Restart**************
C***********Einlesen der mechanischen Kontaktfläche**************
C*** Aufgrund der doppelten Definition der Kontaktflächen exisiteren bei
C*** einer Zweiblechschweißung 6 Kontaktflächen ***     
        FILE_KONTAKTFLAECHE=trim(RestartMechFileName)//'.017' !!
        open(42,file=FILE_KONTAKTFLAECHE, status='UNKNOWN')
        do kontakt=1,6
        read(42,*) kk,KONTAKTFLAECHE(kontakt)
        print*, kk,KONTAKTFLAECHE(kontakt)
        end do !kontakt
        close(42)
C***********Abfrage der aktiven Kontaktflächen**************        
        if(kontaktflaeche(1).eq.0.d0) then
                kontaktflaeche(1)=kontaktflaeche(4)
        end if
        if(kontaktflaeche(2).eq.0.d0) then
                kontaktflaeche(2)=kontaktflaeche(5)
        end if 
        if(kontaktflaeche(3).eq.0.d0) then
                kontaktflaeche(3)=kontaktflaeche(6)
        end if         
C*****************************************************************        

C      Übertrag aus letztem Rechenschritt*************************
       LEITWERT_ELBL1_SPEZ(0)=LEITWERT_ELBL1_SPEZ(ANZAHLITERATIONEN)
       LEITWERT_ELBL2_SPEZ(0)=LEITWERT_ELBL2_SPEZ(ANZAHLITERATIONEN)
       LEITWERT_BLBL_SPEZ(0)=LEITWERT_BLBL_SPEZ(ANZAHLITERATIONEN)
       LEITWERT_ELBL1(0)=LEITWERT_ELBL1(ANZAHLITERATIONEN)
       LEITWERT_ELBL2(0)=LEITWERT_ELBL2(ANZAHLITERATIONEN)
       LEITWERT_BLBL(0)=LEITWERT_BLBL(ANZAHLITERATIONEN)

c        LEITWERT_ELBL1_SPEZ(0)=1000000000000.d0
c        LEITWERT_ELBL2_SPEZ(0)=1000000000000.d0 
c        LEITWERT_BLBL_SPEZ(0)=1000000000000.d0 

    

       if(Inkrement.lt.600) then
       do kk=1,50
       LEITWERT_ELBL1_SPEZ(kk)=0.d0
       LEITWERT_BLBL_SPEZ(kk)=0.d0
       LEITWERT_ELBL2_SPEZ(kk)=0.d0
       end do

       print*,ANZAHLITERATIONEN, LEITWERT_ELBL1_SPEZ(0),
     &LEITWERT_BLBL_SPEZ(0)
c       pause

       do k=1,4

       write(a,*)IntegerIterationElekNumber
       a=adjustl(a)
       StringIterationElekNumber=a

       IterationElekName='_WPS_ELEK_Iteration'
       IterationElekFileName=trim(Datum)//trim(IterationElekName)//
     &trim(StringRestartMechNumber)//'_'//
     &trim(StringIterationElekNumber)
       print*, IterationElekFileName
       IterationElekFileName=adjustl(IterationElekFileName)
        IterationElekFileNameFull=trim(IterationElekFileName)//
     &trim(StringEndungInp)
       print*, IterationElekFileNameFull


C     *****************************************************************
C     ***  Erstellen der Abaqus-Input-Datei (elektrisch-thermisch) ***        
C     *****************************************************************
       open(ITNUMBER, file=trim(IterationELEKFileNameFull),
     &STATUS='UNKNOWN') 
       
        write(ITNUMBER,*)'*Heading'
        write(ITNUMBER,*)'*Preprint, echo=NO, model=NO, history=NO
     &, contact=YES'
        write(ITNUMBER,*)'*Heading'
c        write(ITNUMBER,*)'*Part, name=Part-1'
        write(ITNUMBER,*)'*Node, nset=NALL'
C     *****************************************************************
C     ***  Einlesen des deformierten Netzes aus der durchgeführten 
C          mechanischen Berechnung ***        
C     *****************************************************************           
               write(ITNUMBER,*)'*INCLUDE, input=',
     &trim(RestartMechFileName),'.015'
c     *****************************************************************
C     ***  Elementdefiniton - Vorgabe ***
c     *****************************************************************     
        write(ITNUMBER,*)'*INCLUDE, input=WPS_ELEK_ELEMENTE.txt'
       write(ITNUMBER,*)'**'
       write(ITNUMBER,*)'*INCLUDE, input=WPS_ELEK_Contact.txt'
c       write(ITNUMBER,*)'*Initial Conditions, type=TEMPERATURE'
c       write(ITNUMBER,*)'PART-1-1.NALL, 20.'
c     *****************************************************************
C     ***  Randbedingungen *** 
c     ***************************************************************** 
       if(lauf.eq.1) then
       write(ITNUMBER,*)'*Initial Conditions,type=TEMPERATURE,STEP=1
     &,INC=6,file=',trim(ElekName),'.odb'
       else
       write(ITNUMBER,*)'*Initial Conditions,type=TEMPERATURE,STEP=1
     &,INC=',trim(c),',file=',trim(RestartElekFileNameOld),'.odb'
       end if   
       write(ITNUMBER,*)'*STEP,INC=10000'
       write(ITNUMBER,*)'*COUPLED THERMAL-ELECTRICAL, deltmx=5000'
       write(ITNUMBER,*)'0.001, 0.01, , ,0.01'
       write(ITNUMBER,*)'*FIELD,variable=1,USER'
       write(ITNUMBER,*)'NALL'
       write(ITNUMBER,*)'*BOUNDARY'
       write(ITNUMBER,*)'BC_Elektrode, 9, , 0.0'
       write(ITNUMBER,*)'LOAD_ELEKTRODE, 11, ,20'
       write(ITNUMBER,*)'LOAD_ELEKTRODE_REFERENZ, 11, ,20'
       write(ITNUMBER,*)'BC_Elektrode, 11, , 20'
       write(ITNUMBER,*)'BC_Temp, 11, ,20'
       write(ITNUMBER,*)'*Dsecurrent'
       write(ITNUMBER,*)'SURF_LOAD_CURRENT,CS,', Strom(Inkrement+1)
       write(ITNUMBER,*)'*OUTPUT, FIELD'
       write(ITNUMBER,*)'*ELEMENT OUTPUT'
       write(ITNUMBER,*)'HFL,ECD,EPG,JENER'!, FV'
       write(ITNUMBER,*)'*NODE OUTPUT'
       write(ITNUMBER,*)'NT,EPOT'
C     ******** FIL-Datei zur Ermittlung des elektrischen Potentials****       
       write(ITNUMBER,*)'*NODE FILE'
       write(ITNUMBER,*)'EPOT'
       write(ITNUMBER,*)'*OUTPUT,HISTORY'
       write(ITNUMBER,*)'*ENERGY OUTPUT, VARIABLE=PRESELECT'
       write(ITNUMBER,*)'*END STEP'
       close(ITNUMBER)

c     *****************************************************************
C     ***  Subroutine zur Definiton des elektrischen Kontaktwiderstands
C     ***  Schnittstelle zu Kontaktdruck und Kontaktfläche zur möglichen
C     Einbindung von kontaktdruckabhängigen Formulierungen       
C     *****************************************************************          
       IterationElekNameFIELD='_WPS_FIELD'
       IterationElekFileNameFIELD=trim(Datum)//
     &trim(IterationElekNameFIELD)//
     &trim(StringRestartMechNumber)//'_'//
     &trim(StringIterationElekNumber)
       print*, IterationElekFileNameFIELD
       IterationElekFileNameFIELD=adjustl(IterationElekFileNameFIELD)
       IterationElekFileNameFullFIELD=trim(IterationElekFileNameFIELD)//
     &trim(StringEndungFor)
       print*, IterationElekFileNameFullFIELD


      open(ITNUMBER+1, file=trim(IterationELEKFileNameFullFIELD),
     &STATUS='UNKNOWN')

      write(ITNUMBER+1,*)'     module global'
      write(ITNUMBER+1,*)'     DOUBLE PRECISION ARRAY(1:',Knoten,'),
     & CAREA(1:3)'
      write(ITNUMBER+1,*)'     INTEGER SURFACE(1:3)'
      write(ITNUMBER+1,*)'     SAVE ARRAY, CAREA,SURFACE'
      write(ITNUMBER+1,*)'     end module' 
      write(ITNUMBER+1,*)'     SUBROUTINE UFIELD(FIELD,KFIELD,NSECPT,
     &KSTEP,KINC,TIME,NODE,'
      write(ITNUMBER+1,*)'    1 COORDS,TEMP,DTEMP,NFIELD)'    
      write(ITNUMBER+1,*)'     use global' 
      write(ITNUMBER+1,*)"     INCLUDE 'ABA_PARAM.INC'" 
      write(ITNUMBER+1,*)'     DIMENSION FIELD(NSECPT,NFIELD), TIME(2)
     &, COORDS(3),' 
      write(ITNUMBER+1,*)'    1 TEMP(NSECPT), DTEMP(NSECPT)' 
      write(ITNUMBER+1,*)'     INTEGER Initflag,I,J' 
      write(ITNUMBER+1,*)'     CHARACTER INFILE*255' 

       write(ITNUMBER+1,*)'      CHARACTER INFILE2*255'
       write(ITNUMBER+1,*)'      CHARACTER*256 JOBNAME'
       write(ITNUMBER+1,*)'      CHARACTER xoutdir*255, xfname*80'
       write(ITNUMBER+1,*)'      CHARACTER dmkname*255, fout*255'

      
      write(ITNUMBER+1,*)'     SAVE Initflag,KNOTEN,XCOORD,YCOORD' 
      write(ITNUMBER+1,*)'     INTEGER KNOTEN(1:',Knoten,')'
      write(ITNUMBER+1,*)'     DOUBLE PRECISION XCOORD(1:',Knoten,'),
     & YCOORD(1:',Knoten,')' 
      write(ITNUMBER+1,*)'      call GETJOBNAME( xfname, lxfname )'
      write(ITNUMBER+1,*)'      call GETOUTDIR( xoutdir,lxoutdir)'
      write(ITNUMBER+1,*)"      fout=dmkname(xfname(1:lxfname),xoutdir
     &(1:lxoutdir),'.out')" 

C********************************
C********************************
      write(ITNUMBER+1,*)"      INFILE=trim(fout)//'/",
     &trim(RestartMechfileName),"'//'.016'"
      write(ITNUMBER+1,*)"      INFILE2=trim(fout)//'/",
     &trim(RestartMechfileName),"'//'.017'"
C********************************
      write(ITNUMBER+1,*)'     if(Initflag.eq.0) then' 
      write(ITNUMBER+1,*)"     OPEN(UNIT=16,FILE=INFILE,
     &STATUS='UNKNOWN')" 
      write(ITNUMBER+1,*)'     Do I=1,',Knoten,'' 
      write(ITNUMBER+1,*)'     read(16,*) KNOTEN(I), XCOORD(I),
     & YCOORD(I), ARRAY(I)' 
      write(ITNUMBER+1,*)'     end do' 
      write(ITNUMBER+1,*)"     OPEN(UNIT=17,FILE=INFILE2,
     &STATUS='UNKNOWN')" 
      write(ITNUMBER+1,*)'     DO J=1,3' 
      write(ITNUMBER+1,*)'     read(17,*)SURFACE(J), CAREA(J)' 
      write(ITNUMBER+1,*)'     END DO' 
      write(ITNUMBER+1,*)'     close(16)' 
      write(ITNUMBER+1,*)'     close(17)'
      write(ITNUMBER+1,*)'     Initflag=1' 
      write(ITNUMBER+1,*)'     END IF' 
      write(ITNUMBER+1,*)'     FIELD(1,1)=ARRAY(NODE)'
      write(ITNUMBER+1,*)'     RETURN'
      write(ITNUMBER+1,*)'     END'
      write(ITNUMBER+1,*)'     SUBROUTINE GAPELECTR(SIGMA,D,TEMP,
     &PREDEF,TIME,CINAME,'
      write(ITNUMBER+1,*)'    1 SLNAME,MSNAME,COORDS,NODE,NPRED,KSTEP
     &,KINC)'   
      write(ITNUMBER+1,*)'     use global'
      write(ITNUMBER+1,*)"     INCLUDE 'ABA_PARAM.INC'"
      write(ITNUMBER+1,*)'     CHARACTER*80 CINAME,SLNAME,MSNAME'
      write(ITNUMBER+1,*)'     DIMENSION SIGMA(3),TEMP(2),PREDEF(2,*)
     &,TIME(2),COORDS(2,3)'
      write(ITNUMBER+1,*)'     If(ARRAY(NODE).gt.0.d0.OR.ARRAY(NODE)
     &.LT.0.d0) then'

      write(ITNUMBER+1,*)"     IF(CINAME.eq.'ELEKTRODE') then "
      write(ITNUMBER+1,*)'     SIGMA(1)=',LEITWERT_ELBL1_SPEZ(k-1)
      write(ITNUMBER+1,*)"     else if(CINAME.eq.'BLECH') then"
      write(ITNUMBER+1,*)'     SIGMA(1)=',LEITWERT_BLBL_SPEZ(k-1)
      write(ITNUMBER+1,*)'     END IF'
      write(ITNUMBER+1,*)'      ELSE'
      write(ITNUMBER+1,*)'     SIGMA(1)=0.d0'
      write(ITNUMBER+1,*)'     end if'      
      write(ITNUMBER+1,*)'     RETURN'
      write(ITNUMBER+1,*)'     END'
C******************************************************************************
       write(ITNUMBER+1,*)'      character*(*) FUNCTION dmkname
     &(fname,dname,exten)'
       write(ITNUMBER+1,*)'      character*(*) fname,dname,exten'
       write(ITNUMBER+1,*)'      ltot = len(fname)'
       write(ITNUMBER+1,*)'      lf = 0'
       write(ITNUMBER+1,*)'      DO k1 = ltot,2,-1'
       write(ITNUMBER+1,*)"       if (lf.EQ.0.AND.fname(k1:k1).NE.' ')
     & lf = k1"
       write(ITNUMBER+1,*)'      END DO'
       write(ITNUMBER+1,*)'      ltot = len(dname)'
       write(ITNUMBER+1,*)'      ld = 0'
       write(ITNUMBER+1,*)'      DO k1 = ltot,2,-1'
       write(ITNUMBER+1,*)"       IF (ld.EQ.0.AND.dname(k1:k1).NE.' ')
     &  ld = k1"
       write(ITNUMBER+1,*)'      END DO'
       write(ITNUMBER+1,*)'      ltot = len(exten)'
       write(ITNUMBER+1,*)'      le = 0'
       write(ITNUMBER+1,*)'      DO k1 = ltot,2,-1'
       write(ITNUMBER+1,*)"       IF (le.EQ.0.AND.exten(k1:k1).NE.' ')
     &  le = k1"
       write(ITNUMBER+1,*)'      END DO'
       write(ITNUMBER+1,*)'      IF ((lf+ld+le).LE.len(dmkname)) THEN'
c       write(ITNUMBER+1,*)"      dmkname=dname(1:ld)//'/'//
c     &fname(1:lf)"
       write(ITNUMBER+1,*)"      dmkname=dname(1:ld)"
       write(ITNUMBER+1,*)'      ltot = ld + lf + 1'
       write(ITNUMBER+1,*)'      IF ( le.GT.0) THEN'
c       write(ITNUMBER+1,*)'      dmkname = dmkname(1:ltot)//
c     &exten(1:le)'
       write(ITNUMBER+1,*)'      dmkname = dmkname(1:ltot)'
       write(ITNUMBER+1,*)'      END IF'
       write(ITNUMBER+1,*)'      END IF'
       write(ITNUMBER+1,*)'      RETURN'
       write(ITNUMBER+1,*)'      END'

C     *****************************************************************
C     *** Aufruf elektrisch-thermische Rechnung (Iteration) ***      
C     *****************************************************************           
        AufrufElektrischeRechnung='abq2018 cpus=2  inter job='
     &//trim(IterationElekFileName)//' user='//
     &trim(IterationElekFileNameFIELD)//' scratch=.'

c       IterationElekFileNameOld=trim(RestartElekfileName)
       print*, AufrufElektrischeRechnung
       call system(AufrufElektrischeRechnung)

C     *****************************************************************
C     ***  Erstellung FPERT-Programm zum Auslesen der erstellten
C     Fil-Datei aus der elektrisch-thermischen Berechnung ***
C        *.015-Datei: Knotenkoordinaten des deformierten Netzes und
C        zugehöriges elektrisches Potential
C     *****************************************************************
        FpertName='_WPS_FPERT_EPOT'
       FpertFileName=trim(Datum)//trim(FPERTName)//
     &trim(StringRestartMechNumber)//'_'//  
     &trim(StringIterationElekNumber)
       FPERTNUMBIT=FPERTNUMBIT+1
       print*, FPERTFILENAME
       STRINGENDUNGFOR='.for'
       FpertFileNameFULL=trim(FpertFileName)//StringEndungFor

        open(FPERTNUMBIT, file=trim(FPERTFileNameFull),
     &STATUS='UNKNOWN')

       open(FPERTNUMBIT, file=trim(FPERTFileNameFull),
     &STATUS='UNKNOWN') 
       write(FPERTNUMBIT,*)'     SUBROUTINE ABQMAIN'
       write(FPERTNUMBIT,*)"     INCLUDE 'aba_param.inc'"
       write(FPERTNUMBIT,*)'     DIMENSION  ARRAY(513), JRRAY
     &(NPRECD,513),LRUNIT(2,1)'  
       write(FPERTNUMBIT,*)'     EQUIVALENCE (ARRAY(1), JRRAY(1,1))'
       write(FPERTNUMBIT,*)'     PARAMETER (ITOTAL = ',Knoten,')'
       write(FPERTNUMBIT,*)'     DIMENSION DISP(6,ITOTAL),
     & COORD(3,ITOTAL)'
       write(FPERTNUMBIT,*)'     DIMENSION EPOT(ITOTAL)'
       write(FPERTNUMBIT,*)'     DOUBLE PRECISION CAREA(1:10)'
       write(FPERTNUMBIT,*)'     DIMENSION INODE(ITOTAL), IDOF(30),
     & JEIGNO(10)'
       write(FPERTNUMBIT,*)'     DOUBLE PRECISION DISPLACEMENT
     &(1:',Knoten,',1:2)'
       write(FPERTNUMBIT,*)'     CHARACTER FNAME*80,OUTFILE*(*)'
       write(FPERTNUMBIT,*)"     PARAMETER (OUTFILE = '",
     &trim(IterationElekFileName),".015')"
       write(FPERTNUMBIT,*)'     ICYCLE = 0'
       write(FPERTNUMBIT,*)'     I1901  = 0'
       write(FPERTNUMBIT,*)'     I101   = 0'
       write(FPERTNUMBIT,*)'     I      = 1'
       write(FPERTNUMBIT,*)'     K      = 1'
       write(FPERTNUMBIT,*)'     J      = 0'
       write(FPERTNUMBIT,*)'     NRU = 1'             
       write(FPERTNUMBIT,*)'     LRUNIT(1,NRU) = 8'
       write(FPERTNUMBIT,*)'     LRUNIT(2,NRU) = 2'
       write(FPERTNUMBIT,*)'     LOUTF = 0'
       write(FPERTNUMBIT,*)"     OPEN(UNIT=15,FILE=OUTFILE,STATUS=
     &'UNKNOWN',IOSTAT = J)"
       write(FPERTNUMBIT,*)'     IF (J .NE. 0) THEN'
       write(FPERTNUMBIT,*)'      WRITE(*,*) OUTFILE'
c       write(FPERTNUMBIT,*)'      GOTO 950'
       write(FPERTNUMBIT,*)'     END IF'      
       write(FPERTNUMBIT,*)"     FNAME='",trim(IterationElekFileName),"
     &'"
       write(FPERTNUMBIT,*)'     CALL  INITPF
     & (FNAME, NRU, LRUNIT, LOUTF)'
       write(FPERTNUMBIT,*)'     JUNIT = LRUNIT(1,NRU)'
       write(FPERTNUMBIT,*)'     CALL  DBRNU (JUNIT)'
       write(FPERTNUMBIT,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBIT,*)'     DO WHILE (JRCD .EQ. 0)'
       write(FPERTNUMBIT,*)'     IF (JRRAY(1,2) .EQ. 1921 ) THEN'
       write(FPERTNUMBIT,*)'     NODEMAX = JRRAY(1,8)'
       write(FPERTNUMBIT,*)'     IELMAX  = JRRAY(1,7)'
       write(FPERTNUMBIT,*)'     ICYCLE = ICYCLE +1'
       write(FPERTNUMBIT,*)'     ENDIF'
       write(FPERTNUMBIT,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBIT,*)'     ENDDO'
       write(FPERTNUMBIT,*)'     icycle=1'
       write(FPERTNUMBIT,*)'     CALL DBFILE (2, ARRAY, JRCD)'
       write(FPERTNUMBIT,*)'     WRITE(*,*) NODEMAX, IELMAX'
       write(FPERTNUMBIT,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBIT,*)'     DO WHILE (JRCD .EQ. 0)'
       write(FPERTNUMBIT,*)'     IF (JRRAY(1,2) .EQ. 1901 .AND.
     & ICYCLE .LE. 1) THEN'
       write(FPERTNUMBIT,*)'     I1901 = I1901 + 1'
       write(FPERTNUMBIT,*)'     INODE(I1901)  = JRRAY(1,3)'
       write(FPERTNUMBIT,*)'     COORD(1,I1901) = ARRAY(4)'
       write(FPERTNUMBIT,*)'     COORD(2,I1901) = ARRAY(5)'
       write(FPERTNUMBIT,*)'     COORD(3,I1901) = 0.0D0'
       write(FPERTNUMBIT,*)'     IF (JRRAY(1,1) .GE. 6) COORD(3,I1901)
     & = ARRAY(6)'
       write(FPERTNUMBIT,*)'     ELSE IF (JRRAY(1,2) .EQ. 1902) THEN'
       write(FPERTNUMBIT,*)'     DO 15 IXX = 1, JRRAY(1,1)-2'
       write(FPERTNUMBIT,*)'     IDOF(IXX) = JRRAY(1,IXX+2)'
       write(FPERTNUMBIT,*)'15   CONTINUE'
       write(FPERTNUMBIT,*)'     ITRANS = 3'
       write(FPERTNUMBIT,*)'     IF (IDOF(3) .EQ. 0) ITRANS = 2'
       write(FPERTNUMBIT,*)'     end if'
       write(FPERTNUMBIT,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBIT,*)'     END DO'
       write(FPERTNUMBIT,*)'     CALL DBFILE (2, ARRAY, JRCD)'
       write(FPERTNUMBIT,*)'     icycle=1'
       write(FPERTNUMBIT,*)'     call posfil(1,6,ARRAY,JRCD)'
       write(FPERTNUMBIT,*)'     DO WHILE(JRCD.eq.0)'
       write(FPERTNUMBIT,*)'     IF (JRRAY(1,2) .EQ. 105.and.
     &icycle.le.1) THEN'  
       write(FPERTNUMBIT,*)'     I105 = JRRAY(1,3)'
       write(FPERTNUMBIT,*)'     EPOT(I105)=ARRAY(4)'
       write(FPERTNUMBIT,*)'     END IF'
       write(FPERTNUMBIT,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBIT,*)'     END DO'
       write(FPERTNUMBIT,*)'     WRITE(*,100) OUTFILE'
       write(FPERTNUMBIT,*)"100  FORMAT(//,2X,'The perturbed mesh
     & data are being written to:',"
       write(FPERTNUMBIT,*)'    &1X,A,//)'
       write(FPERTNUMBIT,*)'     DO K = 1, NODEMAX' 
       write(FPERTNUMBIT,*)'     WRITE(15,110) INODE(K), (COORD(J,K),J
     & = 1, 3),'
       write(FPERTNUMBIT,*)'    1 EPOT(INODE(K))'
       write(FPERTNUMBIT,*)"110  FORMAT(I6,4(',',1PE14.6))"    
       write(FPERTNUMBIT,*)'     ENDDO' 
       write(FPERTNUMBIT,*)'     CLOSE (15)'
       write(FPERTNUMBIT,*)'     WRITE(*,120) ' 
       write(FPERTNUMBIT,*)"120  FORMAT(//,2X,' . . . PROGRAM FINISHED
     & SUCCESSFULLY . . . ')"
       write(FPERTNUMBIT,*)'     RETURN'
       write(FPERTNUMBIT,*)'     END'
       close(FPERTNUMBIT)

       AufrufFPERT='abq2018 make job=' // trim(FPERTFILENAME)

       call system(AufrufFpert) 

       AufrufFPERT2='abq2018 ' // trim(FPERTFILENAME)

       call system(AufrufFpert2) 

      ITNUMBER=ITNUMBER+2
      FPERTNUMBIT=FPERTNUMBIT+2      
      IntegerIterationElekNumber=IntegerIterationElekNumber+1


              STRINGENDUNGEPOT='.015'      
        EPOTNAME=trim(IterationElekFileName)//StringEndungEPOT
c        EPOTVALUE=0.d0
        open(123,file=EPOTNAME, status='UNKNOWN')

        do i=1,Knoten
        read(123,*)EPOTNODEAKTUELL, EPOTCOORDAKTUELL(1),
     & EPOTCOORDAKTUELL(2), EPOTCOORDAKTUELL(3), EPOTVALUEAKTUELL
C****** Knoten der Spannungsabgriffe**********************************
C       modellspezifisch zu definieren 
        if(EPOTNODEAKTUELL.EQ.Spannungsabgriff1) then
                EPOTVALUE(k,4)=EPOTVALUEAKTUELL
        else if (EPOTNODEAKTUELL.EQ.Spannungsabgriff2) then       
                EPOTVALUE(k,3)=EPOTVALUEAKTUELL
        else if (EPOTNODEAKTUELL.EQ.Spannungsabgriff3) then       
                EPOTVALUE(k,2)=EPOTVALUEAKTUELL
        else if (EPOTNODEAKTUELL.EQ.Spannungsabgriff4) then       
                EPOTVALUE(k,1)=EPOTVALUEAKTUELL
        end if 
C**********************************************************************      

       end do
       EPOTDIFFERENZ(k,1)=EPOTVALUE(k,1)-EPOTVALUE(k,4)
       EPOTDIFFERENZ(k,2)=EPOTVALUE(k,2)-EPOTVALUE(k,4)
       EPOTDIFFERENZ(k,3)=EPOTVALUE(k,3)-EPOTVALUE(k,4)

      print*,'aktueller Wert:',
     &EPOTVALUE(k,1), EPOTVALUE(k,2),EPOTVALUE(k,3),
     &EPOTVALUE(k,4)
      print*,'Zielwert:',EPOTVALUE(0,1), EPOTVALUE(0,2), EPOTVALUE(0,3),
     &EPOTVALUE(0,4)
c      pause

C       Qualitätsüberprüfung des verwendeten Kontaktleitwertes

       EPOTDIFFERENZ_ELBL1(k)=EPOTVALUE(k,1)-EPOTVALUE(k,2)
       EPOTDIFFERENZ_BLBL(k) =EPOTVALUE(k,2)-EPOTVALUE(k,3)  
       EPOTDIFFERENZ_ELBL2(k)=EPOTVALUE(k,3)-EPOTVALUE(k,4)

C      Temporär Vorgabe hier zu berechnen       

       EPOTDIFFERENZ_ELBL1_VORGABE=EPOTVALUE(0,1)-EPOTVALUE(0,2)
       EPOTDIFFERENZ_BLBL_VORGABE=EPOTVALUE(0,2)-EPOTVALUE(0,3)  
       EPOTDIFFERENZ_ELBL2_VORGABE=EPOTVALUE(0,3)-EPOTVALUE(0,4) 


C     Ermittlung der Vorgaben
      Gesamtstrom=STROM(INKREMENT+1)*CurrentArea

      print*, 'Gesamtstrom [mA]:', Gesamtstrom
      Widerstand1=EPOTDIFFERENZ_ELBL1_VORGABE/Gesamtstrom !gemessener Widerstand
                                                          !zwischen Elektrode und Blech1
      Widerstand2=EPOTDIFFERENZ_BLBL_VORGABE/Gesamtstrom  !gemessener Widerstand
                                                          !zwischen Blech1 und Blech2
      Widerstand3=EPOTDIFFERENZ_ELBL2_VORGABE/Gesamtstrom !gemessener Widerstand
                                                          !zwischen Blech2 und
                                                          !Elektrode

      print*, 'Widerstand1:', Widerstand1    !Ausgabe
      print*, 'Widerstand2:', Widerstand2    !Ausgabe
      print*, 'Widerstand3:', Widerstand3    !Ausgabe


      
C*******************************************************************************************
       QUALITAET_ELBL1(k)= EPOTDIFFERENZ_ELBL1_VORGABE-   !Diff. zwischen gemessenen
     &EPOTDIFFERENZ_ELBL1(k)                              !und berechneten Spannungsverläufen
       QUALITAET_BLBL(k)=EPOTDIFFERENZ_BLBL_VORGABE-
     &EPOTDIFFERENZ_BLBL(k)
       QUALITAET_ELBL2(k)=EPOTDIFFERENZ_ELBL2_VORGABE-
     &EPOTDIFFERENZ_ELBL2(k)

      print*, 'Spannungsdifferenz1:', QUALITAET_ELBL1(k)    !Ausgabe
      print*, 'Spannungsdifferenz2:', QUALITAET_BLBL(k)    !Ausgabe
      print*, 'Spannungsdifferenz3:', QUALITAET_ELBL2(k)    !Ausgabe
C*******************************************************************************************

      DR1=Widerstand1-EPOTDIFFERENZ_ELBL1(k)/Gesamtstrom  !Diff. zwischen gemessenen
                                                          !und berechnetem Widerstand1       

      DR2=Widerstand2-EPOTDIFFERENZ_BLBL(k)/Gesamtstrom  !Diff. zwischen gemessenen
                                                          !und berechnetem Widerstand1

      DR3=Widerstand3-EPOTDIFFERENZ_ELBL2(k)/Gesamtstrom  !Diff. zwischen gemessenen
                                                          !und berechnetem Widerstand1

      print*, 'DR1:', DR1   !Ausgabe
      print*, 'DR2:', DR2   !Ausgabe
      print*, 'DR3:', DR3   !Ausgabe


      Kontaktwiderstand1=1.d0/LEITWERT_ELBL1_SPEZ(k-1)    !spezifischer 
                                                          !Kontaktwiderstand
                                                          !1 (alt)
      Kontaktwiderstand2=1.d0/LEITWERT_BLBL_SPEZ(k-1)     !spezifischer
                                                          !Kontaktwiderstand
                                                          !2 (alt)
      Kontaktwiderstand3=1.d0/LEITWERT_ELBL2_SPEZ(k-1)    !spezifischer
                                                          !Kontaktwiderstand
                                                          !3 (alt)
c********************************************************************************************
 
      print*, 'Kontaktleitwert1:', LEITWERT_ELBL1_SPEZ(k-1)    !Ausgabe
      print*, 'Kontaktleitwert2:', LEITWERT_BLBL_SPEZ(k-1)     !Ausgabe
      print*, 'Kontaktleitwert3:', LEITWERT_ELBL2_SPEZ(k-1)    !Ausgabe  

C********************************************************************************************
      print*, 'Kontaktwiderstand1:', Kontaktwiderstand1   !Ausgabe
      print*, 'Kontaktwiderstand2:', Kontaktwiderstand2   !Ausgabe
      print*, 'Kontaktwiderstand3:', Kontaktwiderstand3   !Ausgabe                                                        


      LEITWERT_ELBL1_SPEZ(k)=
     &((LEITWERT_ELBL1_SPEZ(k-1)*Kontaktflaeche(1)-
     &(Gesamtstrom/QUALITAET_ELBL1(k)))/Kontaktflaeche(1))

      LEITWERT_ELBL1_SPEZ(k)=LEITWERT_ELBL1_SPEZ(k-1)-
     &((Gesamtstrom/QUALITAET_ELBL1(k))/Kontaktflaeche(1))
      LEITWERT_BLBL_SPEZ(k)=LEITWERT_BLBL_SPEZ(k-1)-
     &((Gesamtstrom/QUALITAET_BLBL(k))/Kontaktflaeche(2))
      LEITWERT_ELBL2_SPEZ(k)=LEITWERT_ELBL2_SPEZ(k-1)-
     &((Gesamtstrom/QUALITAET_ELBL2(k))/Kontaktflaeche(3))


      Kontaktwiderstand1=
     &((Kontaktwiderstand1/Kontaktflaeche(1))+DR1)*Kontaktflaeche(1) !Anpassung Kontaktwiderstand1
      if(kontaktwiderstand1.le.0.0000000001) then                    !Widerstand >1.e-8
              kontaktwiderstand1=0.0000000001
      end if        
      Kontaktwiderstand2=
     &((Kontaktwiderstand2/Kontaktflaeche(2))+DR2)*Kontaktflaeche(2) !Anpassung Kontaktwiderstand2
      if(kontaktwiderstand2.le.0.0000000001) then                    !Widerstand >1.e-8                       
              kontaktwiderstand2=0.0000000001
      end if  
      Kontaktwiderstand3=
     &((Kontaktwiderstand3/Kontaktflaeche(3))+DR3)*Kontaktflaeche(3) !Anpassung Kontaktwiderstand3
      if(kontaktwiderstand3.le.0.0000000001) then                    !Widerstand >1.e-8
              kontaktwiderstand3=0.0000000001
      end if  

      print*, 'Kontaktwiderstand1 (Update):', Kontaktwiderstand1   !Ausgabe
      print*, 'Kontaktwiderstand2 (Update):', Kontaktwiderstand2   !Ausgabe
      print*, 'Kontaktwiderstand3 (Update):', Kontaktwiderstand3   !Ausgabe 

      print*, 'Kontaktleitwert1 (Update):', LEITWERT_ELBL1_SPEZ(k)
      print*, 'Kontaktleitwert2 (Update):', LEITWERT_BLBL_SPEZ(k)
      print*, 'Kontaktleitwert3 (Update):', LEITWERT_ELBL2_SPEZ(k)

      LEITWERT_ELBL1_SPEZ(k)=1.d0/Kontaktwiderstand1       !Berechnung
                                                        !spez. Kontaktleitwert
      LEITWERT_BLBL_SPEZ(k)=1.d0/Kontaktwiderstand2        !Berechnung
                                                        !spez. Kontaktleitwert
      LEITWERT_ELBL2_SPEZ(k)=1.d0/Kontaktwiderstand3       !Berechnung
                                                        !spez. Kontaktleitwert 

      print*, 'Kontaktleitwert1 (verwendet):', LEITWERT_ELBL1_SPEZ(k)
      print*, 'Kontaktleitwert2 (verwendet):', LEITWERT_BLBL_SPEZ(k)
      print*, 'Kontaktleitwert3 (verwendet):', LEITWERT_ELBL2_SPEZ(k)


C       Löschung unbenötigter Dateien


       do l=1,7
       if(l.eq.1) then
       DELNAME=trim(IterationElekFileName)//trim(StringEndungCom)
       else if(l.eq.2) then
       DELNAME=trim(IterationElekFileName)//trim(StringEndungFil)
       else if(l.eq.3) then
       DELNAME=trim(IterationElekFileName)//trim(StringEndungMsg)
       else if(l.eq.4) then
       DELNAME=trim(IterationElekFileName)//trim(StringEndungodb)
       else if(l.eq.5) then
       DELNAME=trim(IterationElekFileName)//trim(StringEndungprt)
       else if(l.eq.6) then
       DELNAME=trim(IterationElekFileName)//trim(StringEndungsta)
       else if(l.eq.7) then
       DELNAME=trim(IterationElekFileName)//trim(StringEndungdat)
       end if
       print*, DELNAME
       DEL='del '// trim(DELNAME)  
       call system(DEL)
       end do
       do l=1,2
c       if(l.eq.1) then
c       DELNAME=trim(FPERTFILENAME)//trim(StringEndungfor)
       if(l.eq.1) then
       DELNAME=trim(FPERTFILENAME)//trim(StringEndungobj)
       else if(l.eq.2) then
       DELNAME=trim(FPERTFILENAME)//trim(StringEndungexe)
       end if
       print*, DELNAME
       DEL='del '// trim(DELNAME)  
       call system(DEL)
       end do

C       Abfrage, ob Iteration i.O. 
       ! nicht notwendig
        
        SUMME_QUALITAET(k)=(abs(QUALITAET_ELBL1(k))+
     &abs(QUALITAET_BLBL(k))+
     &abs(QUALITAET_ELBL2(k)))

        if(SUMME_QUALITAET(k).le.(EPOTVALUE(k,1)*0.02d0)) then
                goto 998
        else
        continue
        end if

C       Update-Algorithmus
c             if(k.eq.4) then
c                goto  998
c             else 
c             continue
c             end if   
              
        end do !k=1,4


998     continue   

       ANZAHLITERATIONEN=k-1     

       IterationElekNameZus='_WPS_ELEK_Iteration_ZUS'
       IterationElekNameLW='_WPS_ELEK_Iteration_LW'
      IterationElekFileNameZus=trim(Datum)//trim(IterationElekNameZus)//
     &trim(StringRestartMechNumber)
      IterationElekFileNameLW=trim(Datum)//trim(IterationElekNameLW)//
     &trim(StringRestartMechNumber)
       print*, IterationElekFileNameZus
       print*, IterationElekFileNameLW
       IterationElekFileNameZus=adjustl(IterationElekFileNameZus)
       IterationElekFileNameLW=adjustl(IterationElekFileNameLW)
       IterationElekFileNameZusFull=trim(IterationElekFileNameZus)//
     &trim(StringEndungTXT)
       IterationElekFileNameLWFull=trim(IterationElekFileNameLW)//
     &trim(StringEndungTXT)
       print*, IterationElekFileNameZusFull
       print*, IterationElekFileNameLWFull

       open(77, file=trim(IterationElekFileNameZusFull),
     &STATUS='UNKNOWN')
     
       open(78, file=trim(IterationElekFileNameLWFull),
     &STATUS='UNKNOWN')
         
       do kk=0,50 
       write(77,5)kk,EPOTVALUE(kk,1), EPOTVALUE(kk,2),EPOTVALUE(kk,3),
     &EPOTVALUE(kk,4)
       write(78,5)kk,LEITWERT_ELBL1_SPEZ(kk),LEITWERT_BLBL_SPEZ(kk),
     &LEITWERT_ELBL2_SPEZ(kk) 
       end do
       close(77)
       close(78)             

c       print*, EPOTVALUE
c       print*, k
c       print*, LEITWERT_ELBL1_SPEZ(k-1)
c       pause 
       end if

c**************************************************************** 

       RestartElekName='_WPS_ELEK'
       RestartElekFileName=trim(Datum)//trim(RestartElekName)//
     &trim(StringRestartMechNumber)
       print*, RestartElekFileName
       RestartElekFileName=adjustl(RestartElekFileName)
        RestartElekFileNameFull=trim(RestartElekFileName)//
     &trim(StringEndungInp)
       print*, RestartElekFileNameFull

       open(ELEKNUMBER, file=trim(RestartELEKFileNameFull),
     &STATUS='UNKNOWN')

       print*, k 

       write(ELEKNUMBER,*)'*Heading'
       write(ELEKNUMBER,*)'*Preprint, echo=NO, model=NO, history=NO,
     &contact=YES'
c       write(ELEKNUMBER,*)'*Part, name=Part-1'
       write(ELEKNUMBER,*)'*Node, nset=NALL'
       write(ELEKNUMBER,*)'*INCLUDE, input=',trim(RestartMechFileName),
     &'.015'
       write(ELEKNUMBER,*)'*INCLUDE, input=WPS_ELEK_ELEMENTE.txt'
c       if(IntegerRestartMechNumber.le.19) then 
c       JouleFraction=(IntegerRestartMechNumber+1)*0.05      
c       write(ELEKNUMBER,*)'*JOULE HEAT FRACTION'
c       write(ELEKNUMBER,*)JouleFraction
c       end if
       write(ELEKNUMBER,*)'**'
       write(ELEKNUMBER,*)'*INCLUDE, input=WPS_ELEK_Contact.txt'
       if(lauf.eq.1) then
       write(ELEKNUMBER,*)'*Initial Conditions,type=TEMPERATURE,STEP=1
     &,INC=6,file=',trim(ElekName),'.odb'
       else
       write(ELEKNUMBER,*)'*Initial Conditions,type=TEMPERATURE,STEP=1
     &,INC=',trim(c),',file=',trim(RestartElekFileNameOld),'.odb'
       end if        
       write(ELEKNUMBER,*)'*STEP,INC=10000'
       write(ELEKNUMBER,*)'*COUPLED THERMAL-ELECTRICAL, deltmx=5000'
       if(LAUF.ge.600.AND.LAUF.lt.650)then
       write(ELEKNUMBER,*)'0.001, 0.01,, 0.01'
       else if(lauf.ge.650) then
       write(ELEKNUMBER,*)'0.005, 0.05,, 0.05'
       write(ELEKNUMBER,*)'*MODEL CHANGE, type=CONTACT, remove'
       write(ELEKNUMBER,*)'SURF_BL1_EL1, SURF_EL1_BL1'
       write(ELEKNUMBER,*)'SURF_BL2_EL2, SURF_EL2_BL2'
       else
       write(ELEKNUMBER,*)'0.0001, 0.001,, 0.001'
       end if
       write(ELEKNUMBER,*)'**'
       write(ELEKNUMBER,*)'*FIELD, VARIABLE=1, USER'
       write(ELEKNUMBER,*)'NALL'
       write(ELEKNUMBER,*)'**'
       write(ELEKNUMBER,*)'*BOUNDARY'
       write(ELEKNUMBER,*)'BC_Elektrode, 9, , 0.0'
       write(ELEKNUMBER,*)'LOAD_ELEKTRODE, 11, ,20'
       write(ELEKNUMBER,*)'LOAD_ELEKTRODE_REFERENZ, 11, ,20'
       write(ELEKNUMBER,*)'BC_Elektrode, 11, , 20'
       write(ELEKNUMBER,*)'BC_Temp, 11, ,20'
       write(ELEKNUMBER,*)'*Dsecurrent'
c       if(IntegerRestartMechNumber.le.19) then 
c       JouleFraction=(IntegerRestartMechNumber+1)*0.05*29841  
c       write(ELEKNUMBER,*)'SURF_LOAD_CURRENT,CS,',JouleFraction
       if(INKREMENT.lt.600) then
c       else
       write(ELEKNUMBER,*)'SURF_LOAD_CURRENT,CS,'
     &,Strom(Inkrement+1)
       else
       write(ELEKNUMBER,*)'SURF_LOAD_CURRENT,CS,0.d0'
       end if
       write(ELEKNUMBER,*)'*OUTPUT, FIELD'
       write(ELEKNUMBER,*)'*ELEMENT OUTPUT'
       write(ELEKNUMBER,*)'HFL,ECD,EPG,JENER'!, FV'
       write(ELEKNUMBER,*)'*NODE OUTPUT'
       write(ELEKNUMBER,*)'NT,EPOT'
       write(ELEKNUMBER,*)'*NODE FILE'
       write(ELEKNUMBER,*)'EPOT'
       write(ELEKNUMBER,*)'*OUTPUT,HISTORY'
       write(ELEKNUMBER,*)'*ENERGY OUTPUT, VARIABLE=PRESELECT'
       write(ELEKNUMBER,*)'*END STEP'
       close(ELEKNUMBER)


C******UVARM / GAPCONDUCTANCE****************************
       RestartElekNameFIELD='_WPS_FIELD'
       RestartElekFileNameFIELD=trim(Datum)//
     &trim(RestartElekNameFIELD)//
     &trim(StringRestartMechNumber)
       print*, RestartElekFileNameFIELD
       RestartElekFileNameFIELD=adjustl(RestartElekFileNameFIELD)
        RestartElekFileNameFullFIELD=trim(RestartElekFileNameFIELD)//
     &trim(StringEndungFor)
       print*, RestartElekFileNameFullFIELD

       open(ELEKNUMBER+1, file=trim(RestartELEKFileNameFullFIELD),
     &STATUS='UNKNOWN')

      write(ELEKNUMBER+1,*)'     module global'
      write(ELEKNUMBER+1,*)'     DOUBLE PRECISION ARRAY(1:',Knoten,'),
     & CAREA(1:3)'
      write(ELEKNUMBER+1,*)'     INTEGER SURFACE(1:3)'
      write(ELEKNUMBER+1,*)'     SAVE ARRAY, CAREA,SURFACE'
      write(ELEKNUMBER+1,*)'     end module' 
      write(ELEKNUMBER+1,*)'     SUBROUTINE UFIELD(FIELD,KFIELD,NSECPT,
     &KSTEP,KINC,TIME,NODE,'
      write(ELEKNUMBER+1,*)'    1 COORDS,TEMP,DTEMP,NFIELD)'    
      write(ELEKNUMBER+1,*)'     use global' 
      write(ELEKNUMBER+1,*)"     INCLUDE 'ABA_PARAM.INC'" 
      write(ELEKNUMBER+1,*)'     DIMENSION FIELD(NSECPT,NFIELD), TIME(2)
     &, COORDS(3),' 
      write(ELEKNUMBER+1,*)'    1 TEMP(NSECPT), DTEMP(NSECPT)' 
      write(ELEKNUMBER+1,*)'     INTEGER Initflag,I,J' 
       write(ELEKNUMBER+1,*)'     CHARACTER INFILE*255' 
       write(ELEKNUMBER+1,*)'      CHARACTER INFILE2*255'
       write(ELEKNUMBER+1,*)'      CHARACTER INFILE3*255'
       write(ELEKNUMBER+1,*)'      CHARACTER*256 JOBNAME'
       write(ELEKNUMBER+1,*)'      CHARACTER xoutdir*255, xfname*80'
       write(ELEKNUMBER+1,*)'      CHARACTER dmkname*255, fout*255'
C******************************
c      write(ELEKNUMBER+1,*)"     PARAMETER (INFILE ='D:/temp/
c     &100127_Ausgangsdaten/Vorlage2/"      
c      write(ELEKNUMBER+1,*)"    1",trim(RestartMechfileName),".016')"
c      write(ELEKNUMBER+1,*)"     PARAMETER (INFILE2 ='D:/temp/
c     &100127_Ausgangsdaten/Vorlage2/"
c      write(ELEKNUMBER+1,*)"    1",trim(RestartMechfileName),".017')"
C******************************
c      write(ELEKNUMBER+1,*)"     PARAMETER (INFILE = '",
c     &trim(RestartMechFILEName),".016')"
c      write(ELEKNUMBER+1,*)"     PARAMETER (INFILE2 = '",
c     &trim(RestartMechFILEName),".017')"
C******************************
      
      write(ELEKNUMBER+1,*)'     SAVE Initflag,KNOTEN,XCOORD,YCOORD' 
      write(ELEKNUMBER+1,*)'     INTEGER KNOTEN(1:',Knoten,')'
      write(ELEKNUMBER+1,*)'     DOUBLE PRECISION XCOORD(1:',Knoten,'),
     & YCOORD(1:',Knoten,')' 
      write(ELEKNUMBER+1,*)'      call GETJOBNAME( xfname, lxfname )'
      write(ELEKNUMBER+1,*)'      call GETOUTDIR( xoutdir,lxoutdir)'
      write(ELEKNUMBER+1,*)"      fout=dmkname(xfname(1:lxfname),xoutdir
     &(1:lxoutdir),'.out')" 

C********************************
      write(ELEKNUMBER+1,*)"      INFILE=trim(fout)//'/",
     &trim(RestartMechfileName),"'//'.016'"
      write(ELEKNUMBER+1,*)"      INFILE2=trim(fout)//'/",
     &trim(RestartMechfileName),"'//'.017'"
      write(ELEKNUMBER+1,*)"      INFILE3=trim(fout)//'/",
     &trim(RestartElekfileName),"'//'.out'"   
c      write(ELEKNUMBER+1,*)"     PARAMETER (INFILE = '",
c     &trim(RestartMechFILEName),".016')"
c      write(ELEKNUMBER+1,*)"     PARAMETER (INFILE2 = '",
c     &trim(RestartMechFILEName),".017')"
C********************************

      write(ELEKNUMBER+1,*)'     if(Initflag.eq.0) then' 
      write(ELEKNUMBER+1,*)"     OPEN(UNIT=16,FILE=INFILE,
     &STATUS='UNKNOWN')" 
      write(ELEKNUMBER+1,*)"     OPEN(UNIT=18,FILE=INFILE3,
     &STATUS='UNKNOWN')"       
      write(ELEKNUMBER+1,*)'     Do I=1,',Knoten,'' 
      write(ELEKNUMBER+1,*)'     read(16,*) KNOTEN(I), XCOORD(I),
     & YCOORD(I), ARRAY(I)' 
      write(ELEKNUMBER+1,*)'     end do' 
      write(ELEKNUMBER+1,*)"     OPEN(UNIT=17,FILE=INFILE2,
     &STATUS='UNKNOWN')" 
      write(ELEKNUMBER+1,*)'     DO J=1,3' 
      write(ELEKNUMBER+1,*)'     read(17,*)SURFACE(J), CAREA(J)' 
      write(ELEKNUMBER+1,*)'     END DO' 
      write(ELEKNUMBER+1,*)'     close(16)' 
      write(ELEKNUMBER+1,*)'     close(17)'
      write(ELEKNUMBER+1,*)'     Initflag=1' 
      write(ELEKNUMBER+1,*)'     END IF' 
      write(ELEKNUMBER+1,*)'     FIELD(1,1)=ARRAY(NODE)'
      write(ELEKNUMBER+1,*)'     RETURN'
      write(ELEKNUMBER+1,*)'     END'
      write(ELEKNUMBER+1,*)'     SUBROUTINE GAPELECTR(SIGMA,D,TEMP,
     &PREDEF,TIME,CINAME,'
      write(ELEKNUMBER+1,*)'    1 SLNAME,MSNAME,COORDS,NODE,NPRED,KSTEP
     &,KINC)'   
      write(ELEKNUMBER+1,*)'     use global'
      write(ELEKNUMBER+1,*)"     INCLUDE 'ABA_PARAM.INC'"
      write(ELEKNUMBER+1,*)'     CHARACTER*80 CINAME,SLNAME,MSNAME'
      write(ELEKNUMBER+1,*)'     DIMENSION SIGMA(3),TEMP(2),PREDEF(2,*)
     &,TIME(2),COORDS(2,3)'
      write(ELEKNUMBER+1,*)'     If(ARRAY(NODE).gt.0.d0.OR.ARRAY(NODE)
     &.LT.0.d0) then'
      write(ELEKNUMBER+1,*)"     IF(CINAME.eq.'ELEKTRODE') then "
      if(LAUF.ge.600) then      
      write(ELEKNUMBER+1,*)'     SIGMA(1)=1000000000'
      write(ELEKNUMBER+1,*)"     else if(CINAME.eq.'BLECH') then"
      write(ELEKNUMBER+1,*)'     SIGMA(1)=1000000000'
      write(ELEKNUMBER+1,*)'     END IF'
      else
      write(ELEKNUMBER+1,*)'     SIGMA(1)=',LEITWERT_ELBL1_SPEZ(k-1)
      write(ELEKNUMBER+1,*)"     else if(CINAME.eq.'BLECH') then"
      write(ELEKNUMBER+1,*)'     SIGMA(1)=',LEITWERT_BLBL_SPEZ(k-1)
      write(ELEKNUMBER+1,*)'     END IF'
      end if        
      write(ELEKNUMBER+1,*)'      ELSE'
      write(ELEKNUMBER+1,*)'     SIGMA(1)=0.d0'
      write(ELEKNUMBER+1,*)'     end if'      
      write(ELEKNUMBER+1,*)'     RETURN'
      write(ELEKNUMBER+1,*)'     END'
C******************************************************

      write(ELEKNUMBER+1,*)'      SUBROUTINE UEXTERNALDB(LOP,
     &LRESTART,TIME,DTIME,KSTEP,KINC)'
      write(ELEKNUMBER+1,*)"      INCLUDE 'ABA_PARAM.INC'"
      write(ELEKNUMBER+1,*)'      DIMENSION TIME(2)'
      write(ELEKNUMBER+1,*)'      integer AktuellesINKELEK'  

c      write(ELEKNUMBER+1,*)'      IF(LOP.EQ.0) THEN'
c      write(ELEKNUMBER+1,*)'      open(UNIT=88,'
c      write(ELEKNUMBER+1,*)"    1 file=ELEKINCNAMEFULL, 
c     &STATUS='UNKNOWN')"
c      write(ELEKNUMBER+1,*)'      ENDIF'
      write(ELEKNUMBER+1,*)'      IF(LOP.EQ.3) THEN'
      write(ELEKNUMBER+1,*)'      AktuellesInkELEK=(KINC-1)'                !CHECK!?
      write(ELEKNUMBER+1,*)'      print*, KINC, AktuellesINKELEK'
      write(ELEKNUMBER+1,*)"      write(18,*) AktuellesInkELEk"
      write(ELEKNUMBER+1,*)"      close(18)"
      write(ELEKNUMBER+1,*)'      ENDIF' 
      write(ELEKNUMBER+1,*)'      IF(LOP.EQ.4) THEN'
      write(ELEKNUMBER+1,*)'      AktuellesInkELEK=(KINC-1)'
      write(ELEKNUMBER+1,*)'      print*, KINC, AktuellesINKELEK'
      write(ELEKNUMBER+1,*)"      write(18,*) AktuellesInkELEk"
      write(ELEKNUMBER+1,*)"      close(18)"
      write(ELEKNUMBER+1,*)'      ENDIF'    
      write(ELEKNUMBER+1,*)'      RETURN'
      write(ELEKNUMBER+1,*)'      END'
        
C******************************************************************************
       write(ELEKNUMBER+1,*)'      character*(*) FUNCTION dmkname
     &(fname,dname,exten)'
       write(ELEKNUMBER+1,*)'      character*(*) fname,dname,exten'
       write(ELEKNUMBER+1,*)'      ltot = len(fname)'
       write(ELEKNUMBER+1,*)'      lf = 0'
       write(ELEKNUMBER+1,*)'      DO k1 = ltot,2,-1'
       write(ELEKNUMBER+1,*)"       if (lf.EQ.0.AND.fname(k1:k1).NE.' ')
     & lf = k1"
       write(ELEKNUMBER+1,*)'      END DO'
       write(ELEKNUMBER+1,*)'      ltot = len(dname)'
       write(ELEKNUMBER+1,*)'      ld = 0'
       write(ELEKNUMBER+1,*)'      DO k1 = ltot,2,-1'
       write(ELEKNUMBER+1,*)"       IF (ld.EQ.0.AND.dname(k1:k1).NE.' ')
     &  ld = k1"
       write(ELEKNUMBER+1,*)'      END DO'
       write(ELEKNUMBER+1,*)'      ltot = len(exten)'
       write(ELEKNUMBER+1,*)'      le = 0'
       write(ELEKNUMBER+1,*)'      DO k1 = ltot,2,-1'
       write(ELEKNUMBER+1,*)"       IF (le.EQ.0.AND.exten(k1:k1).NE.' ')
     &  le = k1"
       write(ELEKNUMBER+1,*)'      END DO'
       write(ELEKNUMBER+1,*)'      IF ((lf+ld+le).LE.len(dmkname)) THEN'
c       write(ELEKNUMBER+1,*)"      dmkname=dname(1:ld)//'/'//
c     &fname(1:lf)"
       write(ELEKNUMBER+1,*)"      dmkname=dname(1:ld)"
       write(ELEKNUMBER+1,*)'      ltot = ld + lf + 1'
       write(ELEKNUMBER+1,*)'      IF ( le.GT.0) THEN'
c       write(ELEKNUMBER+1,*)'      dmkname = dmkname(1:ltot)//
c     &exten(1:le)'
       write(ELEKNUMBER+1,*)'      dmkname = dmkname(1:ltot)'
       write(ELEKNUMBER+1,*)'      END IF'
       write(ELEKNUMBER+1,*)'      END IF'
       write(ELEKNUMBER+1,*)'      RETURN'
       write(ELEKNUMBER+1,*)'      END'



C******************************************************************************      
    


      AufrufElektrischeRechnung='abq2018 cpus=2  inter job='
     &//trim(RestartELEKFileName)//' user='//
     &trim(RestartElekFileNameFIELD)//' scratch=.'

       RestartElekFileNameOld=trim(RestartElekfileName)
       print*, AufrufElektrischeRechnung
     
      call system(AufrufElektrischeRechnung)

C******FPERT - EPOT  27.01.10****************************
c**************************************************************** 
       FpertName='_WPS_FPERT_EPOT'
       FpertFileName=trim(Datum)//trim(FPERTName)//
     &trim(StringRestartMechNumber)
       FPERTNUMBER=FPERTNUMBER+1
       print*, FPERTFILENAME
       STRINGENDUNGFOR='.for'
       FpertFileNameFULL=trim(FpertFileName)//StringEndungFor

        open(FPERTNUMBER, file=trim(FPERTFileNameFull),
     &STATUS='UNKNOWN') 
c        open(FPERTNUMBER+1,file=trim(RESTARTMECHFILENAME)//'.out',
c     &STATUS='UNKNOWN')
c        read(FPERTNUMBER+1,*) MECHINK
c        print*, 'MECHINK:', MECHINK
c        close(FPERTNUMBER+1)

       write(FPERTNUMBER,*)'     SUBROUTINE ABQMAIN'
       write(FPERTNUMBER,*)"     INCLUDE 'aba_param.inc'"
       write(FPERTNUMBER,*)'     DIMENSION  ARRAY(513), JRRAY
     &(NPRECD,513),LRUNIT(2,1)'  
       write(FPERTNUMBER,*)'     EQUIVALENCE (ARRAY(1), JRRAY(1,1))'
       write(FPERTNUMBER,*)'     PARAMETER (ITOTAL = ',Knoten,')'
       write(FPERTNUMBER,*)'     DIMENSION DISP(6,ITOTAL),
     & COORD(3,ITOTAL)'
       write(FPERTNUMBER,*)'     DIMENSION EPOT(ITOTAL)'
       write(FPERTNUMBER,*)'     DOUBLE PRECISION CAREA(1:10)'
       write(FPERTNUMBER,*)'     DIMENSION INODE(ITOTAL), IDOF(30),
     & JEIGNO(10)'
       write(FPERTNUMBER,*)'     DOUBLE PRECISION DISPLACEMENT
     &(1:',Knoten,',1:2)'
       write(FPERTNUMBER,*)'     CHARACTER FNAME*80,OUTFILE*(*)'
       write(FPERTNUMBER,*)"     PARAMETER (OUTFILE = '",
     &trim(RestartELEKFILEName),".015')"
       write(FPERTNUMBER,*)'     ICYCLE = 0'
       write(FPERTNUMBER,*)'     I1901  = 0'
       write(FPERTNUMBER,*)'     I101   = 0'
       write(FPERTNUMBER,*)'     I      = 1'
       write(FPERTNUMBER,*)'     K      = 1'
       write(FPERTNUMBER,*)'     J      = 0'
       write(FPERTNUMBER,*)'     NRU = 1'             
       write(FPERTNUMBER,*)'     LRUNIT(1,NRU) = 8'
       write(FPERTNUMBER,*)'     LRUNIT(2,NRU) = 2'
       write(FPERTNUMBER,*)'     LOUTF = 0'
       write(FPERTNUMBER,*)"     OPEN(UNIT=15,FILE=OUTFILE,STATUS=
     &'UNKNOWN',IOSTAT = J)"
       write(FPERTNUMBER,*)'     IF (J .NE. 0) THEN'
       write(FPERTNUMBER,*)'      WRITE(*,*) OUTFILE'
c       write(FPERTNUMBER,*)'      GOTO 950'
       write(FPERTNUMBER,*)'     END IF'      
       write(FPERTNUMBER,*)"     FNAME='",trim(RestartElekfileName),"'"
       write(FPERTNUMBER,*)'     CALL  INITPF
     & (FNAME, NRU, LRUNIT, LOUTF)'
       write(FPERTNUMBER,*)'     JUNIT = LRUNIT(1,NRU)'
       write(FPERTNUMBER,*)'     CALL  DBRNU (JUNIT)'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE (JRCD .EQ. 0)'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,2) .EQ. 1921 ) THEN'
       write(FPERTNUMBER,*)'     NODEMAX = JRRAY(1,8)'
       write(FPERTNUMBER,*)'     IELMAX  = JRRAY(1,7)'
       write(FPERTNUMBER,*)'     ICYCLE = ICYCLE +1'
       write(FPERTNUMBER,*)'     ENDIF'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     ENDDO'
       write(FPERTNUMBER,*)'     icycle=1'
       write(FPERTNUMBER,*)'     CALL DBFILE (2, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     WRITE(*,*) NODEMAX, IELMAX'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE (JRCD .EQ. 0)'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,2) .EQ. 1901 .AND.
     & ICYCLE .LE. 1) THEN'
       write(FPERTNUMBER,*)'     I1901 = I1901 + 1'
       write(FPERTNUMBER,*)'     INODE(I1901)  = JRRAY(1,3)'
       write(FPERTNUMBER,*)'     COORD(1,I1901) = ARRAY(4)'
       write(FPERTNUMBER,*)'     COORD(2,I1901) = ARRAY(5)'
       write(FPERTNUMBER,*)'     COORD(3,I1901) = 0.0D0'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,1) .GE. 6) COORD(3,I1901)
     & = ARRAY(6)'
       write(FPERTNUMBER,*)'     ELSE IF (JRRAY(1,2) .EQ. 1902) THEN'
       write(FPERTNUMBER,*)'     DO 15 IXX = 1, JRRAY(1,1)-2'
       write(FPERTNUMBER,*)'     IDOF(IXX) = JRRAY(1,IXX+2)'
       write(FPERTNUMBER,*)'15   CONTINUE'
       write(FPERTNUMBER,*)'     ITRANS = 3'
       write(FPERTNUMBER,*)'     IF (IDOF(3) .EQ. 0) ITRANS = 2'
       write(FPERTNUMBER,*)'     end if'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     END DO'
       write(FPERTNUMBER,*)'     CALL DBFILE (2, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     icycle=1'
       write(FPERTNUMBER,*)'     call posfil(1,6,ARRAY,JRCD)'
       write(FPERTNUMBER,*)'     DO WHILE(JRCD.eq.0)'
       write(FPERTNUMBER,*)'     IF (JRRAY(1,2) .EQ. 105.and.
     &icycle.le.1) THEN'  
       write(FPERTNUMBER,*)'     I105 = JRRAY(1,3)'
       write(FPERTNUMBER,*)'     EPOT(I105)=ARRAY(4)'
       write(FPERTNUMBER,*)'     END IF'
       write(FPERTNUMBER,*)'     CALL DBFILE (0, ARRAY, JRCD)'
       write(FPERTNUMBER,*)'     END DO'
       write(FPERTNUMBER,*)'     WRITE(*,100) OUTFILE'
       write(FPERTNUMBER,*)"100  FORMAT(//,2X,'The perturbed mesh
     & data are being written to:',"
       write(FPERTNUMBER,*)'    &1X,A,//)'
       write(FPERTNUMBER,*)'     DO K = 1, NODEMAX' 
       write(FPERTNUMBER,*)'     WRITE(15,110) INODE(K), (COORD(J,K),J
     & = 1, 3),'
       write(FPERTNUMBER,*)'    1 EPOT(INODE(K))'
       write(FPERTNUMBER,*)"110  FORMAT(I6,4(',',1PE14.6))"    
       write(FPERTNUMBER,*)'     ENDDO' 
       write(FPERTNUMBER,*)'     CLOSE (15)'
       write(FPERTNUMBER,*)'     WRITE(*,120) ' 
       write(FPERTNUMBER,*)"120  FORMAT(//,2X,' . . . PROGRAM FINISHED
     & SUCCESSFULLY . . . ')"
       write(FPERTNUMBER,*)'     RETURN'
       write(FPERTNUMBER,*)'     END'
       close(FPERTNUMBER)

       AufrufFPERT='abq2018 make job=' // trim(FPERTFILENAME)

       call system(AufrufFpert) 

       AufrufFPERT2='abq2018 ' // trim(FPERTFILENAME)

       call system(AufrufFpert2) 
C*****************************************************************************




      lauf=lauf+1
      step=step+1
      FILENUMBER=FileNumber+2
      FPERTNUMBER=Fpertnumber+2
      ELEKNUMBER=Eleknumber+2
      IntegerRestartMechNumber=IntegerRestartMechNumber+1
      IntegerRestartSTEPNumber=IntegerRestartSTEPNumber+1
      end do



      end program
