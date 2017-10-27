  !===============================================================================
  !                            Len_Str
  !===============================================================================  
    ! This Function Basically Overloads the typical Len Function in order to take 
    ! into account the Str Class
    !
    ! Usage:
    ! 
    ! < Integer >  = Len(<Type Str>)
    !
    ! The Result is the number of Characters that the Object Str Has 
    !
    Pure Function   Len_Str(This) Result(Length)
      Implicit None
      Type(Str), Intent(In) :: This
      Integer               :: Length


      Length = Len(This%Name)
    End  Function   Len_Str