{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parser where
import AST
import Lexer
import ErrM

-- parser produced by Happy Version 1.18.5

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Ident)
	| HappyAbsSyn5 (Integer)
	| HappyAbsSyn6 (Double)
	| HappyAbsSyn7 (String)
	| HappyAbsSyn8 (Program)
	| HappyAbsSyn9 (Arg)
	| HappyAbsSyn10 ([Arg])
	| HappyAbsSyn11 (Definition)
	| HappyAbsSyn12 ([Definition])
	| HappyAbsSyn13 (Block)
	| HappyAbsSyn14 ([Statement])
	| HappyAbsSyn15 (Statement)
	| HappyAbsSyn16 (Declaration)
	| HappyAbsSyn17 ([Declaration])
	| HappyAbsSyn18 (Type)
	| HappyAbsSyn19 ([Type])
	| HappyAbsSyn20 (Expr)
	| HappyAbsSyn28 ([Expr])
	| HappyAbsSyn29 (LBool)
	| HappyAbsSyn30 (AddOp)
	| HappyAbsSyn31 (MulOp)
	| HappyAbsSyn32 (EquOp)
	| HappyAbsSyn33 (RelOp)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

action_0 (54) = happyShift action_7
action_0 (55) = happyShift action_8
action_0 (59) = happyShift action_9
action_0 (62) = happyShift action_10
action_0 (8) = happyGoto action_3
action_0 (11) = happyGoto action_4
action_0 (12) = happyGoto action_5
action_0 (18) = happyGoto action_6
action_0 _ = happyFail

action_1 (67) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (72) = happyAccept
action_3 _ = happyFail

action_4 (54) = happyShift action_7
action_4 (55) = happyShift action_8
action_4 (59) = happyShift action_9
action_4 (62) = happyShift action_10
action_4 (11) = happyGoto action_4
action_4 (12) = happyGoto action_12
action_4 (18) = happyGoto action_6
action_4 _ = happyReduce_11

action_5 _ = happyReduce_5

action_6 (67) = happyShift action_2
action_6 (4) = happyGoto action_11
action_6 _ = happyFail

action_7 _ = happyReduce_31

action_8 _ = happyReduce_30

action_9 _ = happyReduce_29

action_10 _ = happyReduce_32

action_11 (38) = happyShift action_13
action_11 _ = happyFail

action_12 _ = happyReduce_12

action_13 (54) = happyShift action_7
action_13 (55) = happyShift action_8
action_13 (59) = happyShift action_9
action_13 (62) = happyShift action_10
action_13 (9) = happyGoto action_14
action_13 (10) = happyGoto action_15
action_13 (18) = happyGoto action_16
action_13 _ = happyReduce_7

action_14 (43) = happyShift action_19
action_14 _ = happyReduce_8

action_15 (39) = happyShift action_18
action_15 _ = happyFail

action_16 (67) = happyShift action_2
action_16 (4) = happyGoto action_17
action_16 _ = happyFail

action_17 _ = happyReduce_6

action_18 (64) = happyShift action_22
action_18 (13) = happyGoto action_21
action_18 _ = happyFail

action_19 (54) = happyShift action_7
action_19 (55) = happyShift action_8
action_19 (59) = happyShift action_9
action_19 (62) = happyShift action_10
action_19 (9) = happyGoto action_14
action_19 (10) = happyGoto action_20
action_19 (18) = happyGoto action_16
action_19 _ = happyReduce_7

action_20 _ = happyReduce_9

action_21 _ = happyReduce_10

action_22 (14) = happyGoto action_23
action_22 _ = happyReduce_14

action_23 (34) = happyShift action_40
action_23 (38) = happyShift action_41
action_23 (44) = happyShift action_42
action_23 (47) = happyShift action_43
action_23 (54) = happyShift action_7
action_23 (55) = happyShift action_8
action_23 (57) = happyShift action_44
action_23 (58) = happyShift action_45
action_23 (59) = happyShift action_9
action_23 (60) = happyShift action_46
action_23 (61) = happyShift action_47
action_23 (62) = happyShift action_10
action_23 (63) = happyShift action_48
action_23 (64) = happyShift action_22
action_23 (66) = happyShift action_49
action_23 (67) = happyShift action_2
action_23 (68) = happyShift action_50
action_23 (69) = happyShift action_51
action_23 (70) = happyShift action_52
action_23 (4) = happyGoto action_24
action_23 (5) = happyGoto action_25
action_23 (6) = happyGoto action_26
action_23 (7) = happyGoto action_27
action_23 (13) = happyGoto action_28
action_23 (15) = happyGoto action_29
action_23 (18) = happyGoto action_30
action_23 (20) = happyGoto action_31
action_23 (21) = happyGoto action_32
action_23 (22) = happyGoto action_33
action_23 (23) = happyGoto action_34
action_23 (24) = happyGoto action_35
action_23 (25) = happyGoto action_36
action_23 (26) = happyGoto action_37
action_23 (27) = happyGoto action_38
action_23 (29) = happyGoto action_39
action_23 _ = happyFail

action_24 (38) = happyShift action_82
action_24 (42) = happyShift action_83
action_24 (45) = happyShift action_84
action_24 (50) = happyShift action_85
action_24 _ = happyReduce_38

action_25 _ = happyReduce_39

action_26 _ = happyReduce_40

action_27 _ = happyReduce_43

action_28 _ = happyReduce_17

action_29 _ = happyReduce_15

action_30 (67) = happyShift action_2
action_30 (4) = happyGoto action_79
action_30 (16) = happyGoto action_80
action_30 (17) = happyGoto action_81
action_30 _ = happyFail

action_31 _ = happyReduce_47

action_32 _ = happyReduce_49

action_33 (36) = happyShift action_76
action_33 (40) = happyShift action_77
action_33 (46) = happyShift action_78
action_33 (31) = happyGoto action_75
action_33 _ = happyReduce_51

action_34 (41) = happyShift action_73
action_34 (44) = happyShift action_74
action_34 (30) = happyGoto action_72
action_34 _ = happyReduce_54

action_35 (35) = happyShift action_65
action_35 (37) = happyShift action_66
action_35 (48) = happyShift action_67
action_35 (49) = happyShift action_68
action_35 (51) = happyShift action_69
action_35 (52) = happyShift action_70
action_35 (53) = happyShift action_71
action_35 (32) = happyGoto action_63
action_35 (33) = happyGoto action_64
action_35 _ = happyReduce_56

action_36 (65) = happyShift action_62
action_36 _ = happyReduce_58

action_37 _ = happyReduce_60

action_38 (47) = happyShift action_61
action_38 _ = happyFail

action_39 _ = happyReduce_41

action_40 (38) = happyShift action_41
action_40 (57) = happyShift action_44
action_40 (61) = happyShift action_47
action_40 (67) = happyShift action_2
action_40 (68) = happyShift action_50
action_40 (69) = happyShift action_51
action_40 (70) = happyShift action_52
action_40 (4) = happyGoto action_57
action_40 (5) = happyGoto action_25
action_40 (6) = happyGoto action_26
action_40 (7) = happyGoto action_27
action_40 (20) = happyGoto action_60
action_40 (29) = happyGoto action_39
action_40 _ = happyFail

action_41 (34) = happyShift action_40
action_41 (38) = happyShift action_41
action_41 (44) = happyShift action_42
action_41 (57) = happyShift action_44
action_41 (61) = happyShift action_47
action_41 (67) = happyShift action_2
action_41 (68) = happyShift action_50
action_41 (69) = happyShift action_51
action_41 (70) = happyShift action_52
action_41 (4) = happyGoto action_24
action_41 (5) = happyGoto action_25
action_41 (6) = happyGoto action_26
action_41 (7) = happyGoto action_27
action_41 (20) = happyGoto action_31
action_41 (21) = happyGoto action_32
action_41 (22) = happyGoto action_33
action_41 (23) = happyGoto action_34
action_41 (24) = happyGoto action_35
action_41 (25) = happyGoto action_36
action_41 (26) = happyGoto action_37
action_41 (27) = happyGoto action_59
action_41 (29) = happyGoto action_39
action_41 _ = happyFail

action_42 (38) = happyShift action_41
action_42 (57) = happyShift action_44
action_42 (61) = happyShift action_47
action_42 (67) = happyShift action_2
action_42 (68) = happyShift action_50
action_42 (69) = happyShift action_51
action_42 (70) = happyShift action_52
action_42 (4) = happyGoto action_57
action_42 (5) = happyGoto action_25
action_42 (6) = happyGoto action_26
action_42 (7) = happyGoto action_27
action_42 (20) = happyGoto action_58
action_42 (29) = happyGoto action_39
action_42 _ = happyFail

action_43 _ = happyReduce_16

action_44 _ = happyReduce_65

action_45 (38) = happyShift action_56
action_45 _ = happyFail

action_46 (34) = happyShift action_40
action_46 (38) = happyShift action_41
action_46 (44) = happyShift action_42
action_46 (47) = happyShift action_55
action_46 (57) = happyShift action_44
action_46 (61) = happyShift action_47
action_46 (67) = happyShift action_2
action_46 (68) = happyShift action_50
action_46 (69) = happyShift action_51
action_46 (70) = happyShift action_52
action_46 (4) = happyGoto action_24
action_46 (5) = happyGoto action_25
action_46 (6) = happyGoto action_26
action_46 (7) = happyGoto action_27
action_46 (20) = happyGoto action_31
action_46 (21) = happyGoto action_32
action_46 (22) = happyGoto action_33
action_46 (23) = happyGoto action_34
action_46 (24) = happyGoto action_35
action_46 (25) = happyGoto action_36
action_46 (26) = happyGoto action_37
action_46 (27) = happyGoto action_54
action_46 (29) = happyGoto action_39
action_46 _ = happyFail

action_47 _ = happyReduce_64

action_48 (38) = happyShift action_53
action_48 _ = happyFail

action_49 _ = happyReduce_13

action_50 _ = happyReduce_2

action_51 _ = happyReduce_3

action_52 _ = happyReduce_4

action_53 (34) = happyShift action_40
action_53 (38) = happyShift action_41
action_53 (44) = happyShift action_42
action_53 (57) = happyShift action_44
action_53 (61) = happyShift action_47
action_53 (67) = happyShift action_2
action_53 (68) = happyShift action_50
action_53 (69) = happyShift action_51
action_53 (70) = happyShift action_52
action_53 (4) = happyGoto action_24
action_53 (5) = happyGoto action_25
action_53 (6) = happyGoto action_26
action_53 (7) = happyGoto action_27
action_53 (20) = happyGoto action_31
action_53 (21) = happyGoto action_32
action_53 (22) = happyGoto action_33
action_53 (23) = happyGoto action_34
action_53 (24) = happyGoto action_35
action_53 (25) = happyGoto action_36
action_53 (26) = happyGoto action_37
action_53 (27) = happyGoto action_101
action_53 (29) = happyGoto action_39
action_53 _ = happyFail

action_54 (47) = happyShift action_100
action_54 _ = happyFail

action_55 _ = happyReduce_20

action_56 (34) = happyShift action_40
action_56 (38) = happyShift action_41
action_56 (44) = happyShift action_42
action_56 (57) = happyShift action_44
action_56 (61) = happyShift action_47
action_56 (67) = happyShift action_2
action_56 (68) = happyShift action_50
action_56 (69) = happyShift action_51
action_56 (70) = happyShift action_52
action_56 (4) = happyGoto action_24
action_56 (5) = happyGoto action_25
action_56 (6) = happyGoto action_26
action_56 (7) = happyGoto action_27
action_56 (20) = happyGoto action_31
action_56 (21) = happyGoto action_32
action_56 (22) = happyGoto action_33
action_56 (23) = happyGoto action_34
action_56 (24) = happyGoto action_35
action_56 (25) = happyGoto action_36
action_56 (26) = happyGoto action_37
action_56 (27) = happyGoto action_99
action_56 (29) = happyGoto action_39
action_56 _ = happyFail

action_57 (38) = happyShift action_82
action_57 (42) = happyShift action_83
action_57 (45) = happyShift action_84
action_57 _ = happyReduce_38

action_58 _ = happyReduce_45

action_59 (39) = happyShift action_98
action_59 _ = happyFail

action_60 _ = happyReduce_46

action_61 _ = happyReduce_24

action_62 (34) = happyShift action_40
action_62 (38) = happyShift action_41
action_62 (44) = happyShift action_42
action_62 (57) = happyShift action_44
action_62 (61) = happyShift action_47
action_62 (67) = happyShift action_2
action_62 (68) = happyShift action_50
action_62 (69) = happyShift action_51
action_62 (70) = happyShift action_52
action_62 (4) = happyGoto action_57
action_62 (5) = happyGoto action_25
action_62 (6) = happyGoto action_26
action_62 (7) = happyGoto action_27
action_62 (20) = happyGoto action_31
action_62 (21) = happyGoto action_32
action_62 (22) = happyGoto action_33
action_62 (23) = happyGoto action_34
action_62 (24) = happyGoto action_35
action_62 (25) = happyGoto action_36
action_62 (26) = happyGoto action_97
action_62 (29) = happyGoto action_39
action_62 _ = happyFail

action_63 (34) = happyShift action_40
action_63 (38) = happyShift action_41
action_63 (44) = happyShift action_42
action_63 (57) = happyShift action_44
action_63 (61) = happyShift action_47
action_63 (67) = happyShift action_2
action_63 (68) = happyShift action_50
action_63 (69) = happyShift action_51
action_63 (70) = happyShift action_52
action_63 (4) = happyGoto action_57
action_63 (5) = happyGoto action_25
action_63 (6) = happyGoto action_26
action_63 (7) = happyGoto action_27
action_63 (20) = happyGoto action_31
action_63 (21) = happyGoto action_32
action_63 (22) = happyGoto action_33
action_63 (23) = happyGoto action_96
action_63 (29) = happyGoto action_39
action_63 _ = happyFail

action_64 (34) = happyShift action_40
action_64 (38) = happyShift action_41
action_64 (44) = happyShift action_42
action_64 (57) = happyShift action_44
action_64 (61) = happyShift action_47
action_64 (67) = happyShift action_2
action_64 (68) = happyShift action_50
action_64 (69) = happyShift action_51
action_64 (70) = happyShift action_52
action_64 (4) = happyGoto action_57
action_64 (5) = happyGoto action_25
action_64 (6) = happyGoto action_26
action_64 (7) = happyGoto action_27
action_64 (20) = happyGoto action_31
action_64 (21) = happyGoto action_32
action_64 (22) = happyGoto action_33
action_64 (23) = happyGoto action_95
action_64 (29) = happyGoto action_39
action_64 _ = happyFail

action_65 _ = happyReduce_72

action_66 (34) = happyShift action_40
action_66 (38) = happyShift action_41
action_66 (44) = happyShift action_42
action_66 (57) = happyShift action_44
action_66 (61) = happyShift action_47
action_66 (67) = happyShift action_2
action_66 (68) = happyShift action_50
action_66 (69) = happyShift action_51
action_66 (70) = happyShift action_52
action_66 (4) = happyGoto action_57
action_66 (5) = happyGoto action_25
action_66 (6) = happyGoto action_26
action_66 (7) = happyGoto action_27
action_66 (20) = happyGoto action_31
action_66 (21) = happyGoto action_32
action_66 (22) = happyGoto action_33
action_66 (23) = happyGoto action_34
action_66 (24) = happyGoto action_35
action_66 (25) = happyGoto action_94
action_66 (29) = happyGoto action_39
action_66 _ = happyFail

action_67 _ = happyReduce_73

action_68 _ = happyReduce_74

action_69 _ = happyReduce_71

action_70 _ = happyReduce_75

action_71 _ = happyReduce_76

action_72 (34) = happyShift action_40
action_72 (38) = happyShift action_41
action_72 (44) = happyShift action_42
action_72 (57) = happyShift action_44
action_72 (61) = happyShift action_47
action_72 (67) = happyShift action_2
action_72 (68) = happyShift action_50
action_72 (69) = happyShift action_51
action_72 (70) = happyShift action_52
action_72 (4) = happyGoto action_57
action_72 (5) = happyGoto action_25
action_72 (6) = happyGoto action_26
action_72 (7) = happyGoto action_27
action_72 (20) = happyGoto action_31
action_72 (21) = happyGoto action_32
action_72 (22) = happyGoto action_93
action_72 (29) = happyGoto action_39
action_72 _ = happyFail

action_73 _ = happyReduce_66

action_74 _ = happyReduce_67

action_75 (34) = happyShift action_40
action_75 (38) = happyShift action_41
action_75 (44) = happyShift action_42
action_75 (57) = happyShift action_44
action_75 (61) = happyShift action_47
action_75 (67) = happyShift action_2
action_75 (68) = happyShift action_50
action_75 (69) = happyShift action_51
action_75 (70) = happyShift action_52
action_75 (4) = happyGoto action_57
action_75 (5) = happyGoto action_25
action_75 (6) = happyGoto action_26
action_75 (7) = happyGoto action_27
action_75 (20) = happyGoto action_31
action_75 (21) = happyGoto action_92
action_75 (29) = happyGoto action_39
action_75 _ = happyFail

action_76 _ = happyReduce_70

action_77 _ = happyReduce_68

action_78 _ = happyReduce_69

action_79 (50) = happyShift action_91
action_79 _ = happyReduce_25

action_80 (43) = happyShift action_90
action_80 _ = happyReduce_27

action_81 (47) = happyShift action_89
action_81 _ = happyFail

action_82 (34) = happyShift action_40
action_82 (38) = happyShift action_41
action_82 (44) = happyShift action_42
action_82 (57) = happyShift action_44
action_82 (61) = happyShift action_47
action_82 (67) = happyShift action_2
action_82 (68) = happyShift action_50
action_82 (69) = happyShift action_51
action_82 (70) = happyShift action_52
action_82 (4) = happyGoto action_24
action_82 (5) = happyGoto action_25
action_82 (6) = happyGoto action_26
action_82 (7) = happyGoto action_27
action_82 (20) = happyGoto action_31
action_82 (21) = happyGoto action_32
action_82 (22) = happyGoto action_33
action_82 (23) = happyGoto action_34
action_82 (24) = happyGoto action_35
action_82 (25) = happyGoto action_36
action_82 (26) = happyGoto action_37
action_82 (27) = happyGoto action_87
action_82 (28) = happyGoto action_88
action_82 (29) = happyGoto action_39
action_82 _ = happyReduce_61

action_83 _ = happyReduce_36

action_84 _ = happyReduce_37

action_85 (34) = happyShift action_40
action_85 (38) = happyShift action_41
action_85 (44) = happyShift action_42
action_85 (57) = happyShift action_44
action_85 (61) = happyShift action_47
action_85 (67) = happyShift action_2
action_85 (68) = happyShift action_50
action_85 (69) = happyShift action_51
action_85 (70) = happyShift action_52
action_85 (4) = happyGoto action_24
action_85 (5) = happyGoto action_25
action_85 (6) = happyGoto action_26
action_85 (7) = happyGoto action_27
action_85 (20) = happyGoto action_31
action_85 (21) = happyGoto action_32
action_85 (22) = happyGoto action_33
action_85 (23) = happyGoto action_34
action_85 (24) = happyGoto action_35
action_85 (25) = happyGoto action_36
action_85 (26) = happyGoto action_37
action_85 (27) = happyGoto action_86
action_85 (29) = happyGoto action_39
action_85 _ = happyFail

action_86 _ = happyReduce_59

action_87 (43) = happyShift action_107
action_87 _ = happyReduce_62

action_88 (39) = happyShift action_106
action_88 _ = happyFail

action_89 _ = happyReduce_18

action_90 (67) = happyShift action_2
action_90 (4) = happyGoto action_79
action_90 (16) = happyGoto action_80
action_90 (17) = happyGoto action_105
action_90 _ = happyFail

action_91 (34) = happyShift action_40
action_91 (38) = happyShift action_41
action_91 (44) = happyShift action_42
action_91 (57) = happyShift action_44
action_91 (61) = happyShift action_47
action_91 (67) = happyShift action_2
action_91 (68) = happyShift action_50
action_91 (69) = happyShift action_51
action_91 (70) = happyShift action_52
action_91 (4) = happyGoto action_24
action_91 (5) = happyGoto action_25
action_91 (6) = happyGoto action_26
action_91 (7) = happyGoto action_27
action_91 (20) = happyGoto action_31
action_91 (21) = happyGoto action_32
action_91 (22) = happyGoto action_33
action_91 (23) = happyGoto action_34
action_91 (24) = happyGoto action_35
action_91 (25) = happyGoto action_36
action_91 (26) = happyGoto action_37
action_91 (27) = happyGoto action_104
action_91 (29) = happyGoto action_39
action_91 _ = happyFail

action_92 _ = happyReduce_48

action_93 (36) = happyShift action_76
action_93 (40) = happyShift action_77
action_93 (46) = happyShift action_78
action_93 (31) = happyGoto action_75
action_93 _ = happyReduce_50

action_94 _ = happyReduce_55

action_95 (41) = happyShift action_73
action_95 (44) = happyShift action_74
action_95 (30) = happyGoto action_72
action_95 _ = happyReduce_53

action_96 (41) = happyShift action_73
action_96 (44) = happyShift action_74
action_96 (30) = happyGoto action_72
action_96 _ = happyReduce_52

action_97 _ = happyReduce_57

action_98 _ = happyReduce_44

action_99 (39) = happyShift action_103
action_99 _ = happyFail

action_100 _ = happyReduce_19

action_101 (39) = happyShift action_102
action_101 _ = happyFail

action_102 (34) = happyShift action_40
action_102 (38) = happyShift action_41
action_102 (44) = happyShift action_42
action_102 (47) = happyShift action_43
action_102 (54) = happyShift action_7
action_102 (55) = happyShift action_8
action_102 (57) = happyShift action_44
action_102 (58) = happyShift action_45
action_102 (59) = happyShift action_9
action_102 (60) = happyShift action_46
action_102 (61) = happyShift action_47
action_102 (62) = happyShift action_10
action_102 (63) = happyShift action_48
action_102 (64) = happyShift action_22
action_102 (67) = happyShift action_2
action_102 (68) = happyShift action_50
action_102 (69) = happyShift action_51
action_102 (70) = happyShift action_52
action_102 (4) = happyGoto action_24
action_102 (5) = happyGoto action_25
action_102 (6) = happyGoto action_26
action_102 (7) = happyGoto action_27
action_102 (13) = happyGoto action_28
action_102 (15) = happyGoto action_110
action_102 (18) = happyGoto action_30
action_102 (20) = happyGoto action_31
action_102 (21) = happyGoto action_32
action_102 (22) = happyGoto action_33
action_102 (23) = happyGoto action_34
action_102 (24) = happyGoto action_35
action_102 (25) = happyGoto action_36
action_102 (26) = happyGoto action_37
action_102 (27) = happyGoto action_38
action_102 (29) = happyGoto action_39
action_102 _ = happyFail

action_103 (34) = happyShift action_40
action_103 (38) = happyShift action_41
action_103 (44) = happyShift action_42
action_103 (47) = happyShift action_43
action_103 (54) = happyShift action_7
action_103 (55) = happyShift action_8
action_103 (57) = happyShift action_44
action_103 (58) = happyShift action_45
action_103 (59) = happyShift action_9
action_103 (60) = happyShift action_46
action_103 (61) = happyShift action_47
action_103 (62) = happyShift action_10
action_103 (63) = happyShift action_48
action_103 (64) = happyShift action_22
action_103 (67) = happyShift action_2
action_103 (68) = happyShift action_50
action_103 (69) = happyShift action_51
action_103 (70) = happyShift action_52
action_103 (4) = happyGoto action_24
action_103 (5) = happyGoto action_25
action_103 (6) = happyGoto action_26
action_103 (7) = happyGoto action_27
action_103 (13) = happyGoto action_28
action_103 (15) = happyGoto action_109
action_103 (18) = happyGoto action_30
action_103 (20) = happyGoto action_31
action_103 (21) = happyGoto action_32
action_103 (22) = happyGoto action_33
action_103 (23) = happyGoto action_34
action_103 (24) = happyGoto action_35
action_103 (25) = happyGoto action_36
action_103 (26) = happyGoto action_37
action_103 (27) = happyGoto action_38
action_103 (29) = happyGoto action_39
action_103 _ = happyFail

action_104 _ = happyReduce_26

action_105 _ = happyReduce_28

action_106 _ = happyReduce_42

action_107 (34) = happyShift action_40
action_107 (38) = happyShift action_41
action_107 (44) = happyShift action_42
action_107 (57) = happyShift action_44
action_107 (61) = happyShift action_47
action_107 (67) = happyShift action_2
action_107 (68) = happyShift action_50
action_107 (69) = happyShift action_51
action_107 (70) = happyShift action_52
action_107 (4) = happyGoto action_24
action_107 (5) = happyGoto action_25
action_107 (6) = happyGoto action_26
action_107 (7) = happyGoto action_27
action_107 (20) = happyGoto action_31
action_107 (21) = happyGoto action_32
action_107 (22) = happyGoto action_33
action_107 (23) = happyGoto action_34
action_107 (24) = happyGoto action_35
action_107 (25) = happyGoto action_36
action_107 (26) = happyGoto action_37
action_107 (27) = happyGoto action_87
action_107 (28) = happyGoto action_108
action_107 (29) = happyGoto action_39
action_107 _ = happyReduce_61

action_108 _ = happyReduce_63

action_109 (56) = happyShift action_111
action_109 _ = happyReduce_21

action_110 _ = happyReduce_23

action_111 (34) = happyShift action_40
action_111 (38) = happyShift action_41
action_111 (44) = happyShift action_42
action_111 (47) = happyShift action_43
action_111 (54) = happyShift action_7
action_111 (55) = happyShift action_8
action_111 (57) = happyShift action_44
action_111 (58) = happyShift action_45
action_111 (59) = happyShift action_9
action_111 (60) = happyShift action_46
action_111 (61) = happyShift action_47
action_111 (62) = happyShift action_10
action_111 (63) = happyShift action_48
action_111 (64) = happyShift action_22
action_111 (67) = happyShift action_2
action_111 (68) = happyShift action_50
action_111 (69) = happyShift action_51
action_111 (70) = happyShift action_52
action_111 (4) = happyGoto action_24
action_111 (5) = happyGoto action_25
action_111 (6) = happyGoto action_26
action_111 (7) = happyGoto action_27
action_111 (13) = happyGoto action_28
action_111 (15) = happyGoto action_112
action_111 (18) = happyGoto action_30
action_111 (20) = happyGoto action_31
action_111 (21) = happyGoto action_32
action_111 (22) = happyGoto action_33
action_111 (23) = happyGoto action_34
action_111 (24) = happyGoto action_35
action_111 (25) = happyGoto action_36
action_111 (26) = happyGoto action_37
action_111 (27) = happyGoto action_38
action_111 (29) = happyGoto action_39
action_111 _ = happyFail

action_112 _ = happyReduce_22

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn4
		 (Ident happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn5
		 ((read ( happy_var_1)) :: Integer
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn6
		 ((read ( happy_var_1)) :: Double
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn8
		 (Program happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  9 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn9
		 (Arg happy_var_1 happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  10 happyReduction_7
happyReduction_7  =  HappyAbsSyn10
		 ([]
	)

happyReduce_8 = happySpecReduce_1  10 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ((:[]) happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  10 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 6 11 happyReduction_10
happyReduction_10 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Definition happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  12 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 ((:[]) happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  12 happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  13 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Block (reverse happy_var_2)
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_0  14 happyReduction_14
happyReduction_14  =  HappyAbsSyn14
		 ([]
	)

happyReduce_15 = happySpecReduce_2  14 happyReduction_15
happyReduction_15 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  15 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn15
		 (SEmpty
	)

happyReduce_17 = happySpecReduce_1  15 happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn15
		 (SBlock happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  15 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn15
		 (SDeclaration happy_var_1 happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  15 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (SReturn happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  15 happyReduction_20
happyReduction_20 _
	_
	 =  HappyAbsSyn15
		 (SReturnV
	)

happyReduce_21 = happyReduce 5 15 happyReduction_21
happyReduction_21 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (SIf happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 7 15 happyReduction_22
happyReduction_22 ((HappyAbsSyn15  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (SIfElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 5 15 happyReduction_23
happyReduction_23 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (SWhile happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_2  15 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn15
		 (SExpr happy_var_1
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  16 happyReduction_25
happyReduction_25 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 (DNoInit happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  16 happyReduction_26
happyReduction_26 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 (DInit happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  17 happyReduction_27
happyReduction_27 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ((:[]) happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  17 happyReduction_28
happyReduction_28 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  18 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn18
		 (TInt
	)

happyReduce_30 = happySpecReduce_1  18 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn18
		 (TDouble
	)

happyReduce_31 = happySpecReduce_1  18 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn18
		 (TBool
	)

happyReduce_32 = happySpecReduce_1  18 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn18
		 (TVoid
	)

happyReduce_33 = happySpecReduce_0  19 happyReduction_33
happyReduction_33  =  HappyAbsSyn19
		 ([]
	)

happyReduce_34 = happySpecReduce_1  19 happyReduction_34
happyReduction_34 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 ((:[]) happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  19 happyReduction_35
happyReduction_35 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  20 happyReduction_36
happyReduction_36 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn20
		 (EInc happy_var_1
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  20 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn20
		 (EDec happy_var_1
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  20 happyReduction_38
happyReduction_38 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn20
		 (EVar happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  20 happyReduction_39
happyReduction_39 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn20
		 (EInt happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  20 happyReduction_40
happyReduction_40 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn20
		 (EDouble happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  20 happyReduction_41
happyReduction_41 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn20
		 (EBool happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happyReduce 4 20 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (ECall happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_1  20 happyReduction_43
happyReduction_43 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn20
		 (EString happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  20 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  21 happyReduction_45
happyReduction_45 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (ENeg happy_var_2
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  21 happyReduction_46
happyReduction_46 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (ENot happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  21 happyReduction_47
happyReduction_47 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  22 happyReduction_48
happyReduction_48 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EMul happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  22 happyReduction_49
happyReduction_49 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  23 happyReduction_50
happyReduction_50 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EAdd happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  23 happyReduction_51
happyReduction_51 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  24 happyReduction_52
happyReduction_52 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EEqu happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  24 happyReduction_53
happyReduction_53 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn33  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (ERel happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  24 happyReduction_54
happyReduction_54 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  25 happyReduction_55
happyReduction_55 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EAnd happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  25 happyReduction_56
happyReduction_56 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  26 happyReduction_57
happyReduction_57 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EOr happy_var_1 happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  26 happyReduction_58
happyReduction_58 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  27 happyReduction_59
happyReduction_59 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn20
		 (EAss happy_var_1 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  27 happyReduction_60
happyReduction_60 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_0  28 happyReduction_61
happyReduction_61  =  HappyAbsSyn28
		 ([]
	)

happyReduce_62 = happySpecReduce_1  28 happyReduction_62
happyReduction_62 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn28
		 ((:[]) happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  28 happyReduction_63
happyReduction_63 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn28
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  29 happyReduction_64
happyReduction_64 _
	 =  HappyAbsSyn29
		 (LTrue
	)

happyReduce_65 = happySpecReduce_1  29 happyReduction_65
happyReduction_65 _
	 =  HappyAbsSyn29
		 (LFalse
	)

happyReduce_66 = happySpecReduce_1  30 happyReduction_66
happyReduction_66 _
	 =  HappyAbsSyn30
		 (Plus
	)

happyReduce_67 = happySpecReduce_1  30 happyReduction_67
happyReduction_67 _
	 =  HappyAbsSyn30
		 (Minus
	)

happyReduce_68 = happySpecReduce_1  31 happyReduction_68
happyReduction_68 _
	 =  HappyAbsSyn31
		 (Times
	)

happyReduce_69 = happySpecReduce_1  31 happyReduction_69
happyReduction_69 _
	 =  HappyAbsSyn31
		 (Div
	)

happyReduce_70 = happySpecReduce_1  31 happyReduction_70
happyReduction_70 _
	 =  HappyAbsSyn31
		 (Mod
	)

happyReduce_71 = happySpecReduce_1  32 happyReduction_71
happyReduction_71 _
	 =  HappyAbsSyn32
		 (EQU
	)

happyReduce_72 = happySpecReduce_1  32 happyReduction_72
happyReduction_72 _
	 =  HappyAbsSyn32
		 (NE
	)

happyReduce_73 = happySpecReduce_1  33 happyReduction_73
happyReduction_73 _
	 =  HappyAbsSyn33
		 (LTH
	)

happyReduce_74 = happySpecReduce_1  33 happyReduction_74
happyReduction_74 _
	 =  HappyAbsSyn33
		 (LE
	)

happyReduce_75 = happySpecReduce_1  33 happyReduction_75
happyReduction_75 _
	 =  HappyAbsSyn33
		 (GTH
	)

happyReduce_76 = happySpecReduce_1  33 happyReduction_76
happyReduction_76 _
	 =  HappyAbsSyn33
		 (GE
	)

happyNewToken action sts stk [] =
	action 72 72 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 34;
	PT _ (TS _ 2) -> cont 35;
	PT _ (TS _ 3) -> cont 36;
	PT _ (TS _ 4) -> cont 37;
	PT _ (TS _ 5) -> cont 38;
	PT _ (TS _ 6) -> cont 39;
	PT _ (TS _ 7) -> cont 40;
	PT _ (TS _ 8) -> cont 41;
	PT _ (TS _ 9) -> cont 42;
	PT _ (TS _ 10) -> cont 43;
	PT _ (TS _ 11) -> cont 44;
	PT _ (TS _ 12) -> cont 45;
	PT _ (TS _ 13) -> cont 46;
	PT _ (TS _ 14) -> cont 47;
	PT _ (TS _ 15) -> cont 48;
	PT _ (TS _ 16) -> cont 49;
	PT _ (TS _ 17) -> cont 50;
	PT _ (TS _ 18) -> cont 51;
	PT _ (TS _ 19) -> cont 52;
	PT _ (TS _ 20) -> cont 53;
	PT _ (TS _ 21) -> cont 54;
	PT _ (TS _ 22) -> cont 55;
	PT _ (TS _ 23) -> cont 56;
	PT _ (TS _ 24) -> cont 57;
	PT _ (TS _ 25) -> cont 58;
	PT _ (TS _ 26) -> cont 59;
	PT _ (TS _ 27) -> cont 60;
	PT _ (TS _ 28) -> cont 61;
	PT _ (TS _ 29) -> cont 62;
	PT _ (TS _ 30) -> cont 63;
	PT _ (TS _ 31) -> cont 64;
	PT _ (TS _ 32) -> cont 65;
	PT _ (TS _ 33) -> cont 66;
	PT _ (TV happy_dollar_dollar) -> cont 67;
	PT _ (TI happy_dollar_dollar) -> cont 68;
	PT _ (TD happy_dollar_dollar) -> cont 69;
	PT _ (TL happy_dollar_dollar) -> cont 70;
	_ -> cont 71;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 310 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
