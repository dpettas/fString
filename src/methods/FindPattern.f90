
  Function       FindPattern_Char(This,Pattern) Result(OutPut)
    Implicit None 
    Class(Str)      , Intent(In)               :: This
    Character(len=*), Intent(In)               :: Pattern
    Type (Str)                                 :: Work
    Type (Str)      , Dimension(:),Allocatable :: OutPut

    Work = Pattern

    OutPut = This%FindPattern_Str(Work)
  End Function   FindPattern_Char



  Function       FindPattern_Str(This,Pattern) Result(OutPut)
    Implicit None 
    Class(Str), Intent(In)                            :: This
    Type (Str), Intent(In)                            :: Pattern
    Type (Str), Dimension(:),Allocatable              :: OutPut

    Integer                              :: i
    Integer                              :: L
    Integer                              :: U
    Integer                              :: BraKet    
    Integer                              :: LenBraKet
    Integer  , Dimension(:), Allocatable :: PP 
    Type(Str), Dimension(:), Allocatable :: Sub



    BraKet    = Pattern%Count("{}")
    LenBraKet = len("{}") 


    Allocate(PP  (BraKet  )) ; PP      = 0 ! Temporary Solution
    Allocate(Sub (BraKet+1))

    PP  = Pattern%Find  ("{}")  ! Find The Position of BraKet



    Do i = 1, Size(Sub)

      If  (i==1) Then        ; L = 1
      Else                   ; L = PP(i-1) + LenBraKet 
      End if

      If (i==Size(Sub)) Then ; U = len(Pattern)
      Else                   ; U = PP(i)-1
      End If


      Sub(i)= Pattern%Name(L:U)

    End Do


    If (Allocated(OutPut)) DeAllocate(OutPut)
    Allocate(OutPut(Size(Sub)-1))



    Do i = 1, Size(Sub)-1
      If   (i==1) Then ; L =                         1 + len(Sub(i))
      Else             ; L = This%FindOne(Sub(i)%Name) + len(Sub(i))
      End If 

      U = This%FindOne(Sub(i+1)%Name) - 1

      OutPut(i) = This%Name(L:U)

    End Do 
  End Function   FindPattern_Str
  

  Function       Left_Str_Right_Real8 (This,Value) Result(OutPut)
    Implicit None
    Class(Str)      , Intent(In) :: This
    Real(8)         , Intent(In) :: Value
    Type(Str)                    :: OutPut    

    Type(Str)                    :: Work
    Character(len=DefaultLen)    :: Wnum = ''
    Real(8)                      :: Wval
    Integer                      :: Bra
    Integer                      :: ket
    Character(len=:),allocatable :: Fmt
    Character(len=:),allocatable :: Num



    Work = This
    Wval = Value

    Bra  = Work%FindOne("{")
    ket  = Work%FindOne("}")

    Fmt  = Trim(Adjustl(Work%Name(Bra+1:Ket-1)))

    If       (ket == Bra + 1 ) Then  ; Write(Wnum,'('//DefaultFormat//')') Wval 
    Else  If (Ket >  Bra + 1 ) Then  ; Write(Wnum,'('//Fmt          //')') Wval 
    Else  If (fmt == ''      ) Then  ; Write(Wnum,'('//DefaultFormat//')') Wval 
    End   If 


    Num  = Trim(Adjustl(Wnum))

    If      (ket == Bra + 1 ) Then  ; Work = Work%Replace("{}"              ,Num,1)
    Else If (Ket >  Bra + 1 ) Then  ; Work = Work%Replace(Work%Name(Bra:Ket),Num,1)
    End  If
    

    OutPut = Work
  End Function   Left_Str_Right_Real8

  Function       Left_Str_Right_Integer (This,Value) Result(OutPut)
    Implicit None
    Class(Str)      , Intent(In) :: This
    Integer         , Intent(In) :: Value
    Type(Str)                    :: OutPut    

    Type(Str)                    :: Work
    Character(len=DefaultLen)    :: Wnum = ''
    Integer                      :: Wval
    Integer                      :: Bra
    Integer                      :: ket
    Character(len=:),allocatable :: Fmt
    Character(len=:),allocatable :: Num



    Work = This
    Wval = Value

    Bra  = Work%FindOne("{")
    ket  = Work%FindOne("}")

    Fmt  = Trim(Adjustl(Work%Name(Bra+1:Ket-1)))

    If       (ket == Bra + 1 ) Then  ; Write(Wnum,'('//DefaultIntFmt//')') Wval
    Else  If (Ket >  Bra + 1 ) Then  ; Write(Wnum,'('//Fmt          //')') Wval
    Else  If (fmt == ''      ) Then  ; Write(Wnum,'('//DefaultIntFmt//')') Wval
    End   If 



    Num  = Trim(Adjustl(Wnum))

    If      (ket == Bra + 1 ) Then  ; Work = Work%Replace("{}"              ,Num,1)
    Else If (Ket >  Bra + 1 ) Then  ; Work = Work%Replace(Work%Name(Bra:Ket),Num,1)
    End  If
  

    OutPut = Work
  End Function   Left_Str_Right_Integer


  
  Function       Left_Char_Right_Real8 (Ch,Value) Result(OutPut)
    Implicit None
    Character(len=*), Intent(In) :: Ch
    Real(8)         , Intent(In) :: Value
    Type(Str)                    :: OutPut    

    Type(Str)                    :: Work
    Character(len=DefaultLen)    :: Wnum = ''
    Real(8)                      :: Wval
    Integer                      :: Bra
    Integer                      :: ket
    Character(len=:),allocatable :: Fmt
    Character(len=:),allocatable :: Num



    Work = Ch
    Wval = Value

    Bra  = Work%FindOne("{")
    ket  = Work%FindOne("}")

    Fmt  = Trim(Adjustl(Work%Name(Bra+1:Ket-1)))



    If       (ket == Bra + 1 ) Then  ; Write(Wnum,'('//DefaultFormat//')') Wval
    Else  If (Ket >  Bra + 1 ) Then  ; Write(Wnum,'('//Fmt          //')') Wval
    Else  If (fmt == ''      ) Then  ; Write(Wnum,'('//DefaultFormat//')') Wval
    End   If 



    Num  = Trim(Adjustl(Wnum))

    If      (ket == Bra + 1 )  Then  ; Work = Work%Replace("{}"              ,Num,1)
    Else If (Ket >  Bra + 1 )  Then  ; Work = Work%Replace(Work%Name(Bra:Ket),Num,1)
    End  If
      

    OutPut = Work
  End Function   Left_Char_Right_Real8


  Function       Left_Char_Right_Integer (Ch,Value) Result(OutPut)
    Implicit None
    Character(len=*), Intent(In) :: Ch
    Integer         , Intent(In) :: Value
    Type(Str)                    :: OutPut    

    Type(Str)                    :: Work
    Character(len=DefaultLen)    :: Wnum = ''
    Integer                      :: Wval
    Integer                      :: Bra
    Integer                      :: ket
    Character(len=:),allocatable :: Fmt
    Character(len=:),allocatable :: Num



    Work = Ch
    Wval = Value

    Bra  = Work%FindOne("{")  
    ket  = Work%FindOne("}")

    Fmt  = Trim(Adjustl(Work%Name(Bra+1:Ket-1)))



    If       (ket == Bra + 1 ) Then  ; Write(Wnum,'('//DefaultIntFmt//')') Wval 
    Else  If (Ket >  Bra + 1 ) Then  ; Write(Wnum,'('//Fmt          //')') Wval 
    Else  If (fmt == ''      ) Then  ; Write(Wnum,'('//DefaultIntFmt//')') Wval 
    End   If 



    Num  = Trim(Adjustl(Wnum))

    If      (ket == Bra + 1 ) Then  ; Work = Work%Replace("{}"              ,Num,1)
    Else If (Ket >  Bra + 1 ) Then  ; Work = Work%Replace(Work%Name(Bra:Ket),Num,1)
    End  If
    

    OutPut = Work
  End Function   Left_Char_Right_Integer