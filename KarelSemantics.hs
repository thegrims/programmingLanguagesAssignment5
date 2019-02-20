module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState

-- type Robot = (Pos,Card,Int)
pos1 :: Pos
pos1 = (1,1)

rob1 :: Robot
rob1 = (pos1,South,0)

rob2 :: Robot
rob2 = (pos1,North,1)


-- type Defs = [(Macro,Stmt)]
def1 :: Defs
def1 = [("test",Shutdown)]

-- type World = Pos -> Maybe Int
w1 :: World
w1 _ = Just 2

-- testing facing direction working
-- test (Facing South) w1 rob1
-- True

-- testing negation working
-- test (Not (Facing South)) w1 rob1
-- False

-- Testing empty working
-- test Empty w1 rob1
-- True

-- Testing beeper working
-- test Beeper w1 rob1
-- True

-- Testing clear working
-- test (Clear Left) (w1) rob1
-- True


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Facing card) _ robot = (card == (getFacing robot))
test (Not testT) world robot = not (test testT world robot)
test Beeper world robot = hasBeeper (getPos robot) world
test Empty _ robot = isEmpty robot 
test (Clear dir) world robot = isClear (relativePos dir robot) world

-- (cardTurn dir (getFacing robot))

-- Testing PutBeeper
-- stmt PutBeeper def1 w1 rob2
-- OK: ((1,1),South,0)

-- stmt PutBeeper def1 w1 rob1
-- Error: Beeper bag empty

-- Testing Turn Dir
-- stmt (Turn Left) def1 w1 rob1
-- Done: ((1,1),East,0)

-- Testing Move
-- stmt Move def1 w1 rob1
-- Done: ((1,0),South,0)

-- stmt Move def1 w1 rob2
-- Done: ((1,2),North,1)

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r

stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)

stmt PutBeeper _ world robot = 
                        if  not (isEmpty robot)
                            then OK (incBeeper (getPos robot) world) (decBag robot)
                            else Error ("No beeper to put.")

stmt (Turn dir) _ world robot = OK world (updateFacing (cardTurn dir) robot)

stmt Move _ world robot = let futurePos = (neighbor (getFacing robot))
                        in if isClear (futurePos (getPos robot)) world
                            then OK world (updatePos futurePos robot)
                            else Error ("Blocked at: " ++ show (futurePos (getPos robot)))

stmt (Block (statmt:statmts)) defs world robot = let result = stmt statmt defs world robot
                                            in onOK (stmt (Block statmts) defs) result

stmt (Block []) _ world robot = OK world robot      

stmt (If tst statmt statmt2) defs world robot = if (test tst world robot)
                                            then stmt statmt defs world robot
                                            else stmt statmt2 defs world robot

stmt (Call macro) (def:defs) world robot = if (getDefMacro def) == macro
                                            then stmt (getDefStmt def) defs world robot
                                            else stmt (Call macro) defs world robot

stmt (Call macro) [] world robot = Error ("Undefined macro: " ++ macro)

stmt (Iterate 0 statmt) defs world robot = OK world robot

stmt (Iterate count statmt) defs world robot = let result = stmt statmt defs world robot
                                            in onOK (stmt (Iterate (count-1) statmt) defs) result

stmt (While tst statmt) defs world robot = let result = stmt statmt defs world robot
                                        in if (test tst world robot)
                                        then onOK (stmt (While tst statmt) defs) result
                                        else OK world robot 

getDefMacro :: (Macro,Stmt) -> Macro
getDefMacro (macro,_) = macro

getDefStmt :: (Macro,Stmt) -> Stmt
getDefStmt (_,statmt) = statmt

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
