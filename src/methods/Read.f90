


  Subroutine     ReadStrUDIO (This, UNIT, IOTYPE, V_LIST, IOSTAT, IOMSG)

    Class(Str)  , Intent(InOut)      :: This
    Integer     , Intent(In)         :: UNIT
    Character(*), Intent(In)         :: IOTYPE
    INTEGER     , Intent(In)         :: V_LIST (:)
    INTEGER     , Intent(Out)        :: IOSTAT
    Character(*), Intent(InOut)      :: IOMSG

    Character(len=10000)             :: Temp


    Read (Unit, Fmt='(a)', Iostat=Iostat, Iomsg=Iomsg) Temp
    This = Trim(Adjustl(Temp))
  End Subroutine ReadStrUDIO
   
