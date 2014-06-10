module Transport.NISTData where

-- | Represents one line from a NIST cross-section data table
type DataEntry = (Float,Float,Float)

-- | Represents a table of NIST data
type NISTData = [DataEntry]

-- | Parses a file containing NIST data into a lookup function
loadData :: FilePath -> IO (Float -> (Float,Float))
loadData fp = do
    contents <- readFile fp
    let ls = lines contents
    let dat = map (\(x:y:z:[]) -> (1000*read x, read y, read z)) (map words ls)
    return $ nistLookup dat

-- | Finds the two relevant entries to interpolate or errors out
findData :: NISTData -> Float -> (DataEntry,DataEntry)
findData [] _ = error "no data in file!"
findData (d@(x,_,_):d'@(x',_,_):xs) e
    | x <= e && e < x' = (d,d')
    | otherwise        = findData (d':xs) e
findData _ _ = error "Failure in function findData"

-- | Interpolates the cross-section data for the provided energy
nistLookup :: NISTData -> Float -> (Float,Float)
nistLookup dat en
    = let ((x,y,z),(x',y',z')) = findData dat en
          my = (y'-y) / (x'-x)
          mz = (z'-z) / (x'-x)
       in ((en-x)*my+y,((en-x)*mz+z))