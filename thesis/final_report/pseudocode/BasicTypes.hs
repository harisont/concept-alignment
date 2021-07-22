data UDWord = UDWord {
  udID     :: Int,
  udFORM   :: String,
  udLEMMA  :: String, 
  udUPOS   :: String,
  udXPOS   :: String,
  udFEATS  :: [String],
  udHEAD   :: Int,
  udDEPREL :: String,
  udDEPS   :: String,
  udMISC   :: [String] } 

data RTree n = RTree n [RTree n] 

type UDTree = RTree UDWord

type Alignment = (UDTree,UDTree)