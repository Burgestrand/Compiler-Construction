{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Parser where
import AST
import Lexer
import ErrM

-- parser produced by Happy Version 1.18.4

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
 action_112,
 action_113,
 action_114,
 action_115 :: () => Int -> ({-HappyReduction (Err) = -}
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

action_0 (57) = happyShift action_7
action_0 (58) = happyShift action_8
action_0 (62) = happyShift action_9
action_0 (65) = happyShift action_10
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

action_4 (57) = happyShift action_7
action_4 (58) = happyShift action_8
action_4 (62) = happyShift action_9
action_4 (65) = happyShift action_10
action_4 (11) = happyGoto action_4
action_4 (12) = happyGoto action_12
action_4 (18) = happyGoto action_6
action_4 _ = happyReduce_11

action_5 _ = happyReduce_5

action_6 (67) = happyShift action_2
action_6 (4) = happyGoto action_11
action_6 _ = happyFail

action_7 _ = happyReduce_34

action_8 _ = happyReduce_33

action_9 _ = happyReduce_32

action_10 _ = happyReduce_35

action_11 (35) = happyShift action_13
action_11 _ = happyFail

action_12 _ = happyReduce_12

action_13 (57) = happyShift action_7
action_13 (58) = happyShift action_8
action_13 (62) = happyShift action_9
action_13 (65) = happyShift action_10
action_13 (9) = happyGoto action_14
action_13 (10) = happyGoto action_15
action_13 (18) = happyGoto action_16
action_13 _ = happyReduce_7

action_14 (34) = happyShift action_19
action_14 _ = happyReduce_8

action_15 (36) = happyShift action_18
action_15 _ = happyFail

action_16 (67) = happyShift action_2
action_16 (4) = happyGoto action_17
action_16 _ = happyFail

action_17 _ = happyReduce_6

action_18 (37) = happyShift action_22
action_18 (13) = happyGoto action_21
action_18 _ = happyFail

action_19 (57) = happyShift action_7
action_19 (58) = happyShift action_8
action_19 (62) = happyShift action_9
action_19 (65) = happyShift action_10
action_19 (9) = happyGoto action_14
action_19 (10) = happyGoto action_20
action_19 (18) = happyGoto action_16
action_19 _ = happyReduce_7

action_20 _ = happyReduce_9

action_21 _ = happyReduce_10

action_22 (14) = happyGoto action_23
action_22 _ = happyReduce_14

action_23 (35) = happyShift action_40
action_23 (37) = happyShift action_22
action_23 (38) = happyShift action_41
action_23 (39) = happyShift action_42
action_23 (43) = happyShift action_43
action_23 (44) = happyShift action_44
action_23 (57) = happyShift action_7
action_23 (58) = happyShift action_8
action_23 (60) = happyShift action_45
action_23 (61) = happyShift action_46
action_23 (62) = happyShift action_9
action_23 (63) = happyShift action_47
action_23 (64) = happyShift action_48
action_23 (65) = happyShift action_10
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

action_24 (35) = happyShift action_82
action_24 (40) = happyShift action_83
action_24 (41) = happyShift action_84
action_24 (42) = happyShift action_85
action_24 _ = happyReduce_39

action_25 _ = happyReduce_40

action_26 _ = happyReduce_41

action_27 _ = happyReduce_44

action_28 _ = happyReduce_17

action_29 _ = happyReduce_15

action_30 (67) = happyShift action_2
action_30 (4) = happyGoto action_79
action_30 (16) = happyGoto action_80
action_30 (17) = happyGoto action_81
action_30 _ = happyFail

action_31 _ = happyReduce_48

action_32 _ = happyReduce_50

action_33 (48) = happyShift action_76
action_33 (49) = happyShift action_77
action_33 (50) = happyShift action_78
action_33 (31) = happyGoto action_75
action_33 _ = happyReduce_52

action_34 (43) = happyShift action_73
action_34 (47) = happyShift action_74
action_34 (30) = happyGoto action_72
action_34 _ = happyReduce_55

action_35 (45) = happyShift action_65
action_35 (51) = happyShift action_66
action_35 (52) = happyShift action_67
action_35 (53) = happyShift action_68
action_35 (54) = happyShift action_69
action_35 (55) = happyShift action_70
action_35 (56) = happyShift action_71
action_35 (32) = happyGoto action_63
action_35 (33) = happyGoto action_64
action_35 _ = happyReduce_57

action_36 (46) = happyShift action_62
action_36 _ = happyReduce_59

action_37 _ = happyReduce_60

action_38 (39) = happyShift action_61
action_38 _ = happyFail

action_39 _ = happyReduce_42

action_40 (35) = happyShift action_40
action_40 (43) = happyShift action_43
action_40 (44) = happyShift action_44
action_40 (60) = happyShift action_45
action_40 (64) = happyShift action_48
action_40 (67) = happyShift action_2
action_40 (68) = happyShift action_50
action_40 (69) = happyShift action_51
action_40 (70) = happyShift action_52
action_40 (4) = happyGoto action_54
action_40 (5) = happyGoto action_25
action_40 (6) = happyGoto action_26
action_40 (7) = happyGoto action_27
action_40 (20) = happyGoto action_31
action_40 (21) = happyGoto action_32
action_40 (22) = happyGoto action_33
action_40 (23) = happyGoto action_34
action_40 (24) = happyGoto action_35
action_40 (25) = happyGoto action_36
action_40 (26) = happyGoto action_37
action_40 (27) = happyGoto action_60
action_40 (29) = happyGoto action_39
action_40 _ = happyFail

action_41 _ = happyReduce_13

action_42 _ = happyReduce_16

action_43 (35) = happyShift action_40
action_43 (60) = happyShift action_45
action_43 (64) = happyShift action_48
action_43 (67) = happyShift action_2
action_43 (68) = happyShift action_50
action_43 (69) = happyShift action_51
action_43 (70) = happyShift action_52
action_43 (4) = happyGoto action_54
action_43 (5) = happyGoto action_25
action_43 (6) = happyGoto action_26
action_43 (7) = happyGoto action_27
action_43 (20) = happyGoto action_59
action_43 (29) = happyGoto action_39
action_43 _ = happyFail

action_44 (35) = happyShift action_40
action_44 (60) = happyShift action_45
action_44 (64) = happyShift action_48
action_44 (67) = happyShift action_2
action_44 (68) = happyShift action_50
action_44 (69) = happyShift action_51
action_44 (70) = happyShift action_52
action_44 (4) = happyGoto action_54
action_44 (5) = happyGoto action_25
action_44 (6) = happyGoto action_26
action_44 (7) = happyGoto action_27
action_44 (20) = happyGoto action_58
action_44 (29) = happyGoto action_39
action_44 _ = happyFail

action_45 _ = happyReduce_65

action_46 (35) = happyShift action_57
action_46 _ = happyFail

action_47 (35) = happyShift action_40
action_47 (39) = happyShift action_56
action_47 (43) = happyShift action_43
action_47 (44) = happyShift action_44
action_47 (60) = happyShift action_45
action_47 (64) = happyShift action_48
action_47 (67) = happyShift action_2
action_47 (68) = happyShift action_50
action_47 (69) = happyShift action_51
action_47 (70) = happyShift action_52
action_47 (4) = happyGoto action_54
action_47 (5) = happyGoto action_25
action_47 (6) = happyGoto action_26
action_47 (7) = happyGoto action_27
action_47 (20) = happyGoto action_31
action_47 (21) = happyGoto action_32
action_47 (22) = happyGoto action_33
action_47 (23) = happyGoto action_34
action_47 (24) = happyGoto action_35
action_47 (25) = happyGoto action_36
action_47 (26) = happyGoto action_37
action_47 (27) = happyGoto action_55
action_47 (29) = happyGoto action_39
action_47 _ = happyFail

action_48 _ = happyReduce_64

action_49 (35) = happyShift action_53
action_49 _ = happyFail

action_50 _ = happyReduce_2

action_51 _ = happyReduce_3

action_52 _ = happyReduce_4

action_53 (35) = happyShift action_40
action_53 (43) = happyShift action_43
action_53 (44) = happyShift action_44
action_53 (60) = happyShift action_45
action_53 (64) = happyShift action_48
action_53 (67) = happyShift action_2
action_53 (68) = happyShift action_50
action_53 (69) = happyShift action_51
action_53 (70) = happyShift action_52
action_53 (4) = happyGoto action_54
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
action_53 (27) = happyGoto action_103
action_53 (29) = happyGoto action_39
action_53 _ = happyFail

action_54 (35) = happyShift action_82
action_54 _ = happyReduce_39

action_55 (39) = happyShift action_102
action_55 _ = happyFail

action_56 _ = happyReduce_20

action_57 (35) = happyShift action_40
action_57 (43) = happyShift action_43
action_57 (44) = happyShift action_44
action_57 (60) = happyShift action_45
action_57 (64) = happyShift action_48
action_57 (67) = happyShift action_2
action_57 (68) = happyShift action_50
action_57 (69) = happyShift action_51
action_57 (70) = happyShift action_52
action_57 (4) = happyGoto action_54
action_57 (5) = happyGoto action_25
action_57 (6) = happyGoto action_26
action_57 (7) = happyGoto action_27
action_57 (20) = happyGoto action_31
action_57 (21) = happyGoto action_32
action_57 (22) = happyGoto action_33
action_57 (23) = happyGoto action_34
action_57 (24) = happyGoto action_35
action_57 (25) = happyGoto action_36
action_57 (26) = happyGoto action_37
action_57 (27) = happyGoto action_101
action_57 (29) = happyGoto action_39
action_57 _ = happyFail

action_58 _ = happyReduce_47

action_59 _ = happyReduce_46

action_60 (36) = happyShift action_100
action_60 _ = happyFail

action_61 _ = happyReduce_27

action_62 (35) = happyShift action_40
action_62 (43) = happyShift action_43
action_62 (44) = happyShift action_44
action_62 (60) = happyShift action_45
action_62 (64) = happyShift action_48
action_62 (67) = happyShift action_2
action_62 (68) = happyShift action_50
action_62 (69) = happyShift action_51
action_62 (70) = happyShift action_52
action_62 (4) = happyGoto action_54
action_62 (5) = happyGoto action_25
action_62 (6) = happyGoto action_26
action_62 (7) = happyGoto action_27
action_62 (20) = happyGoto action_31
action_62 (21) = happyGoto action_32
action_62 (22) = happyGoto action_33
action_62 (23) = happyGoto action_34
action_62 (24) = happyGoto action_35
action_62 (25) = happyGoto action_36
action_62 (26) = happyGoto action_99
action_62 (29) = happyGoto action_39
action_62 _ = happyFail

action_63 (35) = happyShift action_40
action_63 (43) = happyShift action_43
action_63 (44) = happyShift action_44
action_63 (60) = happyShift action_45
action_63 (64) = happyShift action_48
action_63 (67) = happyShift action_2
action_63 (68) = happyShift action_50
action_63 (69) = happyShift action_51
action_63 (70) = happyShift action_52
action_63 (4) = happyGoto action_54
action_63 (5) = happyGoto action_25
action_63 (6) = happyGoto action_26
action_63 (7) = happyGoto action_27
action_63 (20) = happyGoto action_31
action_63 (21) = happyGoto action_32
action_63 (22) = happyGoto action_33
action_63 (23) = happyGoto action_98
action_63 (29) = happyGoto action_39
action_63 _ = happyFail

action_64 (35) = happyShift action_40
action_64 (43) = happyShift action_43
action_64 (44) = happyShift action_44
action_64 (60) = happyShift action_45
action_64 (64) = happyShift action_48
action_64 (67) = happyShift action_2
action_64 (68) = happyShift action_50
action_64 (69) = happyShift action_51
action_64 (70) = happyShift action_52
action_64 (4) = happyGoto action_54
action_64 (5) = happyGoto action_25
action_64 (6) = happyGoto action_26
action_64 (7) = happyGoto action_27
action_64 (20) = happyGoto action_31
action_64 (21) = happyGoto action_32
action_64 (22) = happyGoto action_33
action_64 (23) = happyGoto action_97
action_64 (29) = happyGoto action_39
action_64 _ = happyFail

action_65 (35) = happyShift action_40
action_65 (43) = happyShift action_43
action_65 (44) = happyShift action_44
action_65 (60) = happyShift action_45
action_65 (64) = happyShift action_48
action_65 (67) = happyShift action_2
action_65 (68) = happyShift action_50
action_65 (69) = happyShift action_51
action_65 (70) = happyShift action_52
action_65 (4) = happyGoto action_54
action_65 (5) = happyGoto action_25
action_65 (6) = happyGoto action_26
action_65 (7) = happyGoto action_27
action_65 (20) = happyGoto action_31
action_65 (21) = happyGoto action_32
action_65 (22) = happyGoto action_33
action_65 (23) = happyGoto action_34
action_65 (24) = happyGoto action_35
action_65 (25) = happyGoto action_96
action_65 (29) = happyGoto action_39
action_65 _ = happyFail

action_66 _ = happyReduce_71

action_67 _ = happyReduce_72

action_68 _ = happyReduce_73

action_69 _ = happyReduce_74

action_70 _ = happyReduce_75

action_71 _ = happyReduce_76

action_72 (35) = happyShift action_40
action_72 (43) = happyShift action_43
action_72 (44) = happyShift action_44
action_72 (60) = happyShift action_45
action_72 (64) = happyShift action_48
action_72 (67) = happyShift action_2
action_72 (68) = happyShift action_50
action_72 (69) = happyShift action_51
action_72 (70) = happyShift action_52
action_72 (4) = happyGoto action_54
action_72 (5) = happyGoto action_25
action_72 (6) = happyGoto action_26
action_72 (7) = happyGoto action_27
action_72 (20) = happyGoto action_31
action_72 (21) = happyGoto action_32
action_72 (22) = happyGoto action_95
action_72 (29) = happyGoto action_39
action_72 _ = happyFail

action_73 _ = happyReduce_67

action_74 _ = happyReduce_66

action_75 (35) = happyShift action_40
action_75 (43) = happyShift action_43
action_75 (44) = happyShift action_44
action_75 (60) = happyShift action_45
action_75 (64) = happyShift action_48
action_75 (67) = happyShift action_2
action_75 (68) = happyShift action_50
action_75 (69) = happyShift action_51
action_75 (70) = happyShift action_52
action_75 (4) = happyGoto action_54
action_75 (5) = happyGoto action_25
action_75 (6) = happyGoto action_26
action_75 (7) = happyGoto action_27
action_75 (20) = happyGoto action_31
action_75 (21) = happyGoto action_94
action_75 (29) = happyGoto action_39
action_75 _ = happyFail

action_76 _ = happyReduce_68

action_77 _ = happyReduce_69

action_78 _ = happyReduce_70

action_79 (42) = happyShift action_93
action_79 _ = happyReduce_28

action_80 (34) = happyShift action_92
action_80 _ = happyReduce_30

action_81 (39) = happyShift action_91
action_81 _ = happyFail

action_82 (35) = happyShift action_40
action_82 (43) = happyShift action_43
action_82 (44) = happyShift action_44
action_82 (60) = happyShift action_45
action_82 (64) = happyShift action_48
action_82 (67) = happyShift action_2
action_82 (68) = happyShift action_50
action_82 (69) = happyShift action_51
action_82 (70) = happyShift action_52
action_82 (4) = happyGoto action_54
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
action_82 (27) = happyGoto action_89
action_82 (28) = happyGoto action_90
action_82 (29) = happyGoto action_39
action_82 _ = happyReduce_61

action_83 (39) = happyShift action_88
action_83 _ = happyFail

action_84 (39) = happyShift action_87
action_84 _ = happyFail

action_85 (35) = happyShift action_40
action_85 (43) = happyShift action_43
action_85 (44) = happyShift action_44
action_85 (60) = happyShift action_45
action_85 (64) = happyShift action_48
action_85 (67) = happyShift action_2
action_85 (68) = happyShift action_50
action_85 (69) = happyShift action_51
action_85 (70) = happyShift action_52
action_85 (4) = happyGoto action_54
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

action_86 (39) = happyShift action_110
action_86 _ = happyFail

action_87 _ = happyReduce_25

action_88 _ = happyReduce_24

action_89 (34) = happyShift action_109
action_89 _ = happyReduce_62

action_90 (36) = happyShift action_108
action_90 _ = happyFail

action_91 _ = happyReduce_18

action_92 (67) = happyShift action_2
action_92 (4) = happyGoto action_79
action_92 (16) = happyGoto action_80
action_92 (17) = happyGoto action_107
action_92 _ = happyFail

action_93 (35) = happyShift action_40
action_93 (43) = happyShift action_43
action_93 (44) = happyShift action_44
action_93 (60) = happyShift action_45
action_93 (64) = happyShift action_48
action_93 (67) = happyShift action_2
action_93 (68) = happyShift action_50
action_93 (69) = happyShift action_51
action_93 (70) = happyShift action_52
action_93 (4) = happyGoto action_54
action_93 (5) = happyGoto action_25
action_93 (6) = happyGoto action_26
action_93 (7) = happyGoto action_27
action_93 (20) = happyGoto action_31
action_93 (21) = happyGoto action_32
action_93 (22) = happyGoto action_33
action_93 (23) = happyGoto action_34
action_93 (24) = happyGoto action_35
action_93 (25) = happyGoto action_36
action_93 (26) = happyGoto action_37
action_93 (27) = happyGoto action_106
action_93 (29) = happyGoto action_39
action_93 _ = happyFail

action_94 _ = happyReduce_49

action_95 (48) = happyShift action_76
action_95 (49) = happyShift action_77
action_95 (50) = happyShift action_78
action_95 (31) = happyGoto action_75
action_95 _ = happyReduce_51

action_96 _ = happyReduce_56

action_97 (43) = happyShift action_73
action_97 (47) = happyShift action_74
action_97 (30) = happyGoto action_72
action_97 _ = happyReduce_54

action_98 (43) = happyShift action_73
action_98 (47) = happyShift action_74
action_98 (30) = happyGoto action_72
action_98 _ = happyReduce_53

action_99 _ = happyReduce_58

action_100 _ = happyReduce_45

action_101 (36) = happyShift action_105
action_101 _ = happyFail

action_102 _ = happyReduce_19

action_103 (36) = happyShift action_104
action_103 _ = happyFail

action_104 (35) = happyShift action_40
action_104 (37) = happyShift action_22
action_104 (39) = happyShift action_42
action_104 (43) = happyShift action_43
action_104 (44) = happyShift action_44
action_104 (57) = happyShift action_7
action_104 (58) = happyShift action_8
action_104 (60) = happyShift action_45
action_104 (61) = happyShift action_46
action_104 (62) = happyShift action_9
action_104 (63) = happyShift action_47
action_104 (64) = happyShift action_48
action_104 (65) = happyShift action_10
action_104 (66) = happyShift action_49
action_104 (67) = happyShift action_2
action_104 (68) = happyShift action_50
action_104 (69) = happyShift action_51
action_104 (70) = happyShift action_52
action_104 (4) = happyGoto action_24
action_104 (5) = happyGoto action_25
action_104 (6) = happyGoto action_26
action_104 (7) = happyGoto action_27
action_104 (13) = happyGoto action_28
action_104 (15) = happyGoto action_113
action_104 (18) = happyGoto action_30
action_104 (20) = happyGoto action_31
action_104 (21) = happyGoto action_32
action_104 (22) = happyGoto action_33
action_104 (23) = happyGoto action_34
action_104 (24) = happyGoto action_35
action_104 (25) = happyGoto action_36
action_104 (26) = happyGoto action_37
action_104 (27) = happyGoto action_38
action_104 (29) = happyGoto action_39
action_104 _ = happyFail

action_105 (35) = happyShift action_40
action_105 (37) = happyShift action_22
action_105 (39) = happyShift action_42
action_105 (43) = happyShift action_43
action_105 (44) = happyShift action_44
action_105 (57) = happyShift action_7
action_105 (58) = happyShift action_8
action_105 (60) = happyShift action_45
action_105 (61) = happyShift action_46
action_105 (62) = happyShift action_9
action_105 (63) = happyShift action_47
action_105 (64) = happyShift action_48
action_105 (65) = happyShift action_10
action_105 (66) = happyShift action_49
action_105 (67) = happyShift action_2
action_105 (68) = happyShift action_50
action_105 (69) = happyShift action_51
action_105 (70) = happyShift action_52
action_105 (4) = happyGoto action_24
action_105 (5) = happyGoto action_25
action_105 (6) = happyGoto action_26
action_105 (7) = happyGoto action_27
action_105 (13) = happyGoto action_28
action_105 (15) = happyGoto action_112
action_105 (18) = happyGoto action_30
action_105 (20) = happyGoto action_31
action_105 (21) = happyGoto action_32
action_105 (22) = happyGoto action_33
action_105 (23) = happyGoto action_34
action_105 (24) = happyGoto action_35
action_105 (25) = happyGoto action_36
action_105 (26) = happyGoto action_37
action_105 (27) = happyGoto action_38
action_105 (29) = happyGoto action_39
action_105 _ = happyFail

action_106 _ = happyReduce_29

action_107 _ = happyReduce_31

action_108 _ = happyReduce_43

action_109 (35) = happyShift action_40
action_109 (43) = happyShift action_43
action_109 (44) = happyShift action_44
action_109 (60) = happyShift action_45
action_109 (64) = happyShift action_48
action_109 (67) = happyShift action_2
action_109 (68) = happyShift action_50
action_109 (69) = happyShift action_51
action_109 (70) = happyShift action_52
action_109 (4) = happyGoto action_54
action_109 (5) = happyGoto action_25
action_109 (6) = happyGoto action_26
action_109 (7) = happyGoto action_27
action_109 (20) = happyGoto action_31
action_109 (21) = happyGoto action_32
action_109 (22) = happyGoto action_33
action_109 (23) = happyGoto action_34
action_109 (24) = happyGoto action_35
action_109 (25) = happyGoto action_36
action_109 (26) = happyGoto action_37
action_109 (27) = happyGoto action_89
action_109 (28) = happyGoto action_111
action_109 (29) = happyGoto action_39
action_109 _ = happyReduce_61

action_110 _ = happyReduce_26

action_111 _ = happyReduce_63

action_112 (59) = happyShift action_114
action_112 _ = happyReduce_21

action_113 _ = happyReduce_23

action_114 (35) = happyShift action_40
action_114 (37) = happyShift action_22
action_114 (39) = happyShift action_42
action_114 (43) = happyShift action_43
action_114 (44) = happyShift action_44
action_114 (57) = happyShift action_7
action_114 (58) = happyShift action_8
action_114 (60) = happyShift action_45
action_114 (61) = happyShift action_46
action_114 (62) = happyShift action_9
action_114 (63) = happyShift action_47
action_114 (64) = happyShift action_48
action_114 (65) = happyShift action_10
action_114 (66) = happyShift action_49
action_114 (67) = happyShift action_2
action_114 (68) = happyShift action_50
action_114 (69) = happyShift action_51
action_114 (70) = happyShift action_52
action_114 (4) = happyGoto action_24
action_114 (5) = happyGoto action_25
action_114 (6) = happyGoto action_26
action_114 (7) = happyGoto action_27
action_114 (13) = happyGoto action_28
action_114 (15) = happyGoto action_115
action_114 (18) = happyGoto action_30
action_114 (20) = happyGoto action_31
action_114 (21) = happyGoto action_32
action_114 (22) = happyGoto action_33
action_114 (23) = happyGoto action_34
action_114 (24) = happyGoto action_35
action_114 (25) = happyGoto action_36
action_114 (26) = happyGoto action_37
action_114 (27) = happyGoto action_38
action_114 (29) = happyGoto action_39
action_114 _ = happyFail

action_115 _ = happyReduce_22

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn4
		 (Ident happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn5
		 ((read happy_var_1) :: Integer
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn6
		 ((read happy_var_1) :: Double
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

happyReduce_24 = happySpecReduce_3  15 happyReduction_24
happyReduction_24 _
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn15
		 (SInc happy_var_1
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  15 happyReduction_25
happyReduction_25 _
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn15
		 (SDec happy_var_1
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 4 15 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (SAss happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_2  15 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn15
		 (SExpr happy_var_1
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 (DNoInit happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  16 happyReduction_29
happyReduction_29 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 (DInit happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  17 happyReduction_30
happyReduction_30 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ((:[]) happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  17 happyReduction_31
happyReduction_31 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  18 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn18
		 (TInt
	)

happyReduce_33 = happySpecReduce_1  18 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn18
		 (TDouble
	)

happyReduce_34 = happySpecReduce_1  18 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn18
		 (TBool
	)

happyReduce_35 = happySpecReduce_1  18 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn18
		 (TVoid
	)

happyReduce_36 = happySpecReduce_0  19 happyReduction_36
happyReduction_36  =  HappyAbsSyn19
		 ([]
	)

happyReduce_37 = happySpecReduce_1  19 happyReduction_37
happyReduction_37 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 ((:[]) happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  19 happyReduction_38
happyReduction_38 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  20 happyReduction_39
happyReduction_39 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn20
		 (EVar happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  20 happyReduction_40
happyReduction_40 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn20
		 (EInt happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  20 happyReduction_41
happyReduction_41 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn20
		 (EDouble happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  20 happyReduction_42
happyReduction_42 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn20
		 (EBool happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happyReduce 4 20 happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (ECall happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_1  20 happyReduction_44
happyReduction_44 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn20
		 (EString happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  20 happyReduction_45
happyReduction_45 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  21 happyReduction_46
happyReduction_46 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (ENeg happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  21 happyReduction_47
happyReduction_47 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (ENot happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  21 happyReduction_48
happyReduction_48 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  22 happyReduction_49
happyReduction_49 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EMul happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  22 happyReduction_50
happyReduction_50 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  23 happyReduction_51
happyReduction_51 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EAdd happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  23 happyReduction_52
happyReduction_52 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  24 happyReduction_53
happyReduction_53 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EEqu happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  24 happyReduction_54
happyReduction_54 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn33  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (ERel happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  24 happyReduction_55
happyReduction_55 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  25 happyReduction_56
happyReduction_56 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EAnd happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  25 happyReduction_57
happyReduction_57 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  26 happyReduction_58
happyReduction_58 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EOr happy_var_1 happy_var_3
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  26 happyReduction_59
happyReduction_59 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

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
	PT _ (TS ",") -> cont 34;
	PT _ (TS "(") -> cont 35;
	PT _ (TS ")") -> cont 36;
	PT _ (TS "{") -> cont 37;
	PT _ (TS "}") -> cont 38;
	PT _ (TS ";") -> cont 39;
	PT _ (TS "++") -> cont 40;
	PT _ (TS "--") -> cont 41;
	PT _ (TS "=") -> cont 42;
	PT _ (TS "-") -> cont 43;
	PT _ (TS "!") -> cont 44;
	PT _ (TS "&&") -> cont 45;
	PT _ (TS "||") -> cont 46;
	PT _ (TS "+") -> cont 47;
	PT _ (TS "*") -> cont 48;
	PT _ (TS "/") -> cont 49;
	PT _ (TS "%") -> cont 50;
	PT _ (TS "==") -> cont 51;
	PT _ (TS "!=") -> cont 52;
	PT _ (TS "<") -> cont 53;
	PT _ (TS "<=") -> cont 54;
	PT _ (TS ">") -> cont 55;
	PT _ (TS ">=") -> cont 56;
	PT _ (TS "boolean") -> cont 57;
	PT _ (TS "double") -> cont 58;
	PT _ (TS "else") -> cont 59;
	PT _ (TS "false") -> cont 60;
	PT _ (TS "if") -> cont 61;
	PT _ (TS "int") -> cont 62;
	PT _ (TS "return") -> cont 63;
	PT _ (TS "true") -> cont 64;
	PT _ (TS "void") -> cont 65;
	PT _ (TS "while") -> cont 66;
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
  Bad $ "syntax error at " ++ tokenPos ts ++ if null ts then [] else (" before " ++ unwords (map prToken (take 4 ts)))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}








{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

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

{-# LINE 155 "templates/GenericTemplate.hs" #-}

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
     let i = (case x of { HappyErrorToken (i) -> i }) in
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
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates/GenericTemplate.hs" #-}
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

{-# LINE 317 "templates/GenericTemplate.hs" #-}
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
