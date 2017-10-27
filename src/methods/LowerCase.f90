
  Function       LowerCase ( This ) Result ( OutPut )
     ! -- Argument and result
     Class(Str), Intent(in)     :: This
     Type (Str)                 :: OutPut
     ! -- Local variables
     Character(len=:),Allocatable :: StrIn
     Character(len=:),Allocatable :: StrOut
     Integer                      :: i, n

     StrIn  = This%Name
     StrOut = StrIn
     
     StrOut = StrIn ! -- Copy input string
     
     Do i = 1, Len( StrOut )                          ! -- Loop over string elements       
       n = INDEX( UPPER_CASE, StrOut( i:i ) )         ! -- Find location of letter in lower case constant string       
       IF ( n /= 0 ) StrOut( i:i ) = LOWER_CASE( n:n )! -- If current substring is a lower case letter, make it upper case
     End Do

     OutPut%Name = StrOut 
  End Function   LowerCase
