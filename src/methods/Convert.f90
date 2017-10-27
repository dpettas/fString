
  Subroutine     Str2Real8_Sub(OutPut,This) 
    Implicit None 
    Real(8)         , Intent(Out)            :: OutPut
    Class(Str)      , Intent(In)             :: This


    Character(Len=DefaultLen)                      :: String 


  
    String = Trim(Adjustl(This%Name))
    ! If it Is Numeric convert the number else set output equal to NaN
    
    If (This%IsNumeric()) Then ; Read(String,'(f16.8)'   ) OutPut
    Else                       ; OutPut = 0.d0 ; OutPut = 0.d0/OutPut
    End If


    ! Auto einai ena Error ths Fortran
    IF ( This%Count(".") == 0) OutPut = OutPut * 1.D+8 




  End Subroutine Str2Real8_Sub

  Subroutine     Str2Int_Sub  (OutPut,This)
      Implicit None 
      Integer         , Intent(Out)            :: OutPut
      Class(Str)      , Intent(In)             :: This

      Character(Len=DefaultLen)                :: String 


      String = Trim(Adjustl(This%Name))

      If(This%Count("0.")/=0) Then
      OutPut = 0 ; OutPut = 0/OutPut
      Return
      End If 



      If(This%IsNumeric()) Then ; Read(String,'(i8.8)' ) OutPut
      Else                      ; OutPut = 0 ; OutPut = 0/OutPut
      End If 
  End Subroutine Str2Int_Sub


  Subroutine     Str2Char_Sub (OutPut,This)
    Implicit None
    Class(Str)                   , Intent(In)  :: This    
    Character(len=:), Allocatable, Intent(Out) :: OutPut


    OutPut = This%Name
  End Subroutine Str2Char_Sub




  Function       Str2Real8(This,Format) Result(OutPut)
    Implicit None 
    Class(Str)      , Intent(In)             :: This
    Character(len=*), Intent(In)   ,Optional :: Format
    Real(8)                                  :: OutPut

    Character(Len=DefaultLen)                :: String 


  
    String = This%Name

  
  
    If (Present(Format)) Then ; Read(This%Name, Format  ) OutPut
    Else                      ; Read(String,'(f12.4)'   ) OutPut
    End If


    IF ( This%Count(".") == 0) OutPut = OutPut * 10000.d0 
  End Function   Str2Real8

  Function       Str2Int(This,Format) Result(OutPut)
      Implicit None 
      Class(Str)      , Intent(InOut)          :: This
      Character(len=*), Intent(In)   ,Optional :: Format
      Integer                                  :: OutPut



      If (Present(Format)) Then ; Read(This%Name, Format  ) OutPut
      Else                      ; Read(This%Name,'(i8.8)' ) OutPut
      End If
  End Function   Str2Int

