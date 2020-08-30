{-# OPTIONS -Wall #-}

module Main where

{- 15.3 -}
(-:) :: a -> (a -> b) -> b
x -: f = f x

type Name = String

type Data = String

data FSItem = File Name Data | Folder Name [FSItem]
  deriving (Show)

myDisk :: FSItem
myDisk =
  Folder
    "root"
    [ File "goat_yelling_like_man.wmv" "baaaaaa",
      File "pope_time.avi" "god bless",
      Folder
        "pics"
        [ File "ape_throwing_up.jpg" "bleargh",
          File "watermelon_smash.gif" "smash!!",
          File "skull_man(scary).bmp" "Yikes!"
        ],
      File "dijon_poupon.doc" "best mustard",
      Folder
        "programs"
        [ File "fartwizard.exe" "10gotofart",
          File "owl_bandit.dmg" "mov eax, h00t",
          File "not_a_virus.exe" "really not a virus",
          Folder
            "source code"
            [ File "best_hs_prog.hs" "main = print (fix error)",
              File "random.hs" "main = print 4"
            ]
        ]
    ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (current, (FSCrumb root prev next) : fscs) = (Folder root (prev ++ [current] ++ next), fscs)

-- >>> let newFocus = (myDisk, []) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"
-- >>> let newFocus2 = newFocus -: fsUp -: fsTo "watermelon_smash.gif"
-- >>> fst newFocus2
-- File "watermelon_smash.gif" "smash!!"
fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, fscs) =
  let (prev, item : next) = break (nameIs name) items
   in (item, (FSCrumb folderName prev next) : fscs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

-- >>> (myDisk, []) -: fsTo "pics" -: fsRename "cspi" -: fsUp
-- (Folder "root" [File "goat_yelling_like_man.wmv" "baaaaaa",File "pope_time.avi" "god bless",Folder "cspi" [File "ape_throwing_up.jpg" "bleargh",File "watermelon_smash.gif" "smash!!",File "skull_man(scary).bmp" "Yikes!"],File "dijon_poupon.doc" "best mustard",Folder "programs" [File "fartwizard.exe" "10gotofart",File "owl_bandit.dmg" "mov eax, h00t",File "not_a_virus.exe" "really not a virus",Folder "source code" [File "best_hs_prog.hs" "main = print (fix error)",File "random.hs" "main = print 4"]]],[])
fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder _ items, fscs) = (Folder newName items, fscs)
fsRename newName (File _ dat, fscs) = (File newName dat, fscs)

-- >>> (myDisk, []) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp
-- (Folder "root" [File "goat_yelling_like_man.wmv" "baaaaaa",File "pope_time.avi" "god bless",Folder "pics" [File "heh.jpg" "lol",File "ape_throwing_up.jpg" "bleargh",File "watermelon_smash.gif" "smash!!",File "skull_man(scary).bmp" "Yikes!"],File "dijon_poupon.doc" "best mustard",Folder "programs" [File "fartwizard.exe" "10gotofart",File "owl_bandit.dmg" "mov eax, h00t",File "not_a_virus.exe" "really not a virus",Folder "source code" [File "best_hs_prog.hs" "main = print (fix error)",File "random.hs" "main = print 4"]]],[])
fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, fscs) =
  (Folder folderName (item : items), fscs)
