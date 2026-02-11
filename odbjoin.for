        program odbjoin
      
c     'Datum'_MECH_JOINED.odb muss händisch als Kopie von 'Datum'_MECH_START.odb erzeugt werden
      
        integer i
        integer IntegerRestartMechNumber
        character (len=256) AufrufOdbJoin
        Character(len=30)  Datum
        character (len=40) StringRestartMechNumber
        character (len=40) RestartMechFileName
        character (len=100) RestartMechName
        character*40 a
      
        IntegerRestartMechNumber=1
        Datum='250822'
      
        do i=0, 179
        
        write(a,*)IntegerRestartMechnumber    
        
        a=adjustl(a)
      
        StringRestartMechNumber=a  
        
        RestartMechName='_WPS_MECH_RESTART'
        RestartMechFileName=trim(Datum)//trim(RestartMechName)//
     &trim(StringRestartMechNumber)//'.odb'
        print*, RestartMechFileName
        
        
c        RestartMechFileName='210315_WPS_MECH_RESTART'//trim(x)//'.odb'
        
        AufrufOdbJoin='abq2018 restartjoin originalodb='
     &//trim(Datum)//'_MECH_JOINED.odb restartodb='
     &//trim(RestartMechFileName)
     
      print*, AufrufOdbJoin
      
      call system(AufrufOdbJoin)
     
c       history
     
      IntegerRestartMechNumber=IntegerRestartMechNumber+1
      
        end do
        
        end program  