



Program Main 

  Use fString
  Implicit None
  !define the New Object of the Class Str from the module fString
  Type(Str) :: Lyrics

  !> Initi1ize the Lyrics object with a alpharithetic
  !> (lose yourself Eminem)
  Lyrics = "Look If you had One shot Or one opportunity  To seize everything you ever wanted In one moment Would you capture Or just let it slip?"

  !> Convert the string to Capital Letters
  Lyrics = Lyrics%UpperCase()


  Print*, '=================================================='
  Print*, 'Print UpperCase/LowerCase of the Lyrics'
  Print*, '=================================================='
  Print*, ''
  Print*, ''
  !>Print Direct the object or Print the Function 

  Print*, 'UpperCase   : ',Lyrics
  Print*, 'LowerCase   : ',Lyrics%LowerCase()

End Program Main 



