main = interact $ const "ok!"

--data VarID a = VarID String --ok


--type VarID a = a VarID --no

type VarID = String   
class Register a where  --okay!
    allocate :: a VarID
