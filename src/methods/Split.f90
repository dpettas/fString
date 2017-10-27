
  Function     Split(This,Sep) Result(OutPut)
    Implicit None 
    Class(Str)      , Intent(In)          :: This
    Character(len=*), Intent(In), Optional:: Sep
    Type (Str), Dimension(:), Allocatable :: OutPut

    Type (Str)                            :: Temp
    Integer                               :: NumSpaces
    Integer, Dimension(:), Allocatable    :: PosSpace 
    Integer                               :: Iword
    Integer                               :: IL, IU
    Integer, Dimension(:), Allocatable    :: LowRange
    Integer, Dimension(:), Allocatable    ::  UpRange
  
  
    Temp      = This%CutSpaces()
    If(Present(Sep)) Then 
    NumSpaces = Temp%Count   (sep)
    PosSpace  = Temp%Find    (sep)
  
    Else 
    NumSpaces   = Temp%Count   (" ")
    PosSpace  = Temp%Find      (" ")  
    End If
    If (Allocated(OutPut)) DeAllocate(OutPut)
  
    Allocate(OutPut  (NumSpaces+1))
  
    If (NumSpaces == 0 ) Then ! One Word
    If (Allocated(OutPut)) DeAllocate(OutPut)
    Allocate(OutPut  (1))
    OutPut(1) = Temp%Name
    Return
    End If
  
    Allocate(LowRange(NumSpaces+1))
    Allocate( UpRange(NumSpaces+1))

    LowRange = (/0+1          ,PosSpace(:)+1 /)
     UpRange = (/PosSpace(:)-1,Len(Temp)     /)

    Do Iword = 1 , NumSpaces+1
    IL = LowRange(Iword)
    IU =  UpRange(Iword)
    OutPut(Iword) = Temp%Name(IL:IU)
    End Do
  End Function Split
