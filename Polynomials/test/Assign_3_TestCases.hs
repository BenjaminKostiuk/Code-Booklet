{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 101
 - - Input: (Coef 0) 0 
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 102
 - - Input: (Coef 3) 0
 - - Expected Output: 3
 - - Acutal Output: 3
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 103
 - - Input: (Coef 3) 2
 - - Expected Output: 3
 - - Acutal Output: 3
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 104
 - - Input: X 2
 - - Expected Output: 2
 - - Acutal Output: 2
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 105
 - - Input: X (-0.5)
 - - Expected Output: -0.5
 - - Acutal Output: -0.5
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 106
 - - Input: (Sum X X) 2
 - - Expected Output: 4
 - - Acutal Output: 4
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 107
 - - Input: (Sum (Coef (-2)) X) 2
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 108
 - - Input: (Sum (Coef 2) (Coef 2)) 7
 - - Expected Output: 4
 - - Acutal Output: 4
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 109
 - - Input: (Prod X X) 6
 - - Expected Output: 36
 - - Acutal Output: 36
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 110
 - - Input: (Prod (Coef 0) X) 5
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 111
 - - Input: (Prod (Coef 1) X) 5
 - - Expected Output: 5
 - - Acutal Output: 5
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 112
 - - Input: (Prod (Coef 3) (Coef (-3))) 5
 - - Expected Output: -9
 - - Acutal Output: -9
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 113
 - - Input: (Prod X (Prod X (Prod X (Prod X (Coef (-3)))))) 4
 - - Expected Output: -768
 - - Acutal Output: -768
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 114
 - - Input: (Sum (Prod X (Prod X (Coef 4))) (Prod X (Prod X (Coef (-4))))) 3
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 115
 - - Input: (Prod (Sum X (Coef (-1))) (Sum X (Coef 1))) (-4)
 - - Expected Output: 15
 - - Acutal Output: 15
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 116
 - - Input: Coef 0
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 117
 - - Input: Coef 2
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 118
 - - Input: X
 - - Expected Output: 1
 - - Acutal Output: 1
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 119
 - - Input: Sum (Coef 3) X
 - - Expected Output: 1
 - - Acutal Output: 1
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 120
 - - Input: Prod (Coef 0) X
 - - Expected Output: Undefined
 - - Acutal Output: 1
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 121
 - - Input: Prod (Coef 3) X
 - - Expected Output: 1
 - - Acutal Output: 1
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 122
 - - Input: Sum (Prod X X) X
 - - Expected Output: 2
 - - Acutal Output: 2
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 123
 - - Input: Prod (Sum (Coef 3) X) (Sum (Coef 3) X)
 - - Expected Output: 2
 - - Acutal Output: 2
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 124
 - - Input: Sum (Prod (Prod X X) X) (Prod X (Prod X (Prod X X))) 
 - - Expected Output: 4
 - - Acutal Output: 4
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 125
 - - Input: (Coef 0)
 - - Expected Output: Coef 0
 - - Acutal Output: Coef 0
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 126
 - - Input: (Coef 3)
 - - Expected Output: Coef 0
 - - Acutal Output: Coef 0
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 127
 - - Input: (Coef (-1.5))
 - - Expected Output: Coef 0.0
 - - Acutal Output: Coef 0.0
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 128
 - - Input: X
 - - Expected Output: Coef 1
 - - Acutal Output: Coef 1
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 129
 - - Input: (Sum X X)
 - - Expected Output: Coef 2
 - - Acutal Output: Sum (Coef 1) (Coef 1)
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 130
 - - Input: (Sum X (Coef 2))
 - - Expected Output: Coef 1
 - - Acutal Output: Sum (Coef 1) (Coef 0)
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 131
 - - Input: (Sum (Coef 2) (Coef (-4))) 
 - - Expected Output: Coef 0
 - - Acutal Output: Sum (Coef 0) (Coef 0)
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 132
 - - Input: (Prod X X)
 - - Expected Output: Prod (Coef 2) X
 - - Acutal Output: Sum (Prod (Coef 1) X) (Prod X (Coef 1))
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 133
 - - Input: (Prod (Coef 3) X)
 - - Expected Output: Coef 3
 - - Acutal Output: Sum (Prod (Coef 0) X) (Prod (Coef 3) (Coef 1))
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 134
 - - Input: (Prod (Coef 3) (Coef (-3))) 
 - - Expected Output: Coef 0
 - - Acutal Output: Sum (Prod (Coef 0) (Coef (-3))) (Prod (Coef 3) (Coef 0))
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 135
 - - Input: (Sum (Prod (Coef 2) X) (Prod (Coef (-4)) (Prod X X)))
 - - Expected Output: Sum (Coef 2) (Prod (Coef (-8)) X)
 - - Acutal Output: Sum (Sum (Prod (Coef 0) X) (Prod (Coef 2) (Coef 1))) (Sum (Prod (Coef 0) (Prod X X)) (Prod (Coef (-4)) (Sum (Prod (Coef 1) X) (Prod X (Coef 1)))))
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 136
 - - Input: (Prod (Sum X (Coef (-1))) (Sum X (Coef 1)))
 - - Expected Output: Prod (Coef 2) X
 - - Acutal Output: Sum (Prod (Sum (Coef 1) (Coef 0)) (Sum X (Coef 1))) (Prod (Sum X (Coef (-1))) (Sum (Coef 1) (Coef 0)))
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: getPolyList (Empty text file)
 - - Test Case Number: 101
 - - Input: "../test/testCoeffs.txt"
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []
 - -----------------------------------------------------------------
 - - Function: getPolyList (Contains 2,5,7 on different lines)
 - - Test Case Number: 102
 - - Input: "../test/testCoeffs.txt"
 - - Expected Output: PolyList [2,5,7]
 - - Acutal Output: PolyList [2,5,7]
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 104
 - - Input: (PolyList []) 0
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 105
 - - Input: (PolyList []) 2
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 106
 - - Input: (PolyList [1]) 0
 - - Expected Output: 1
 - - Acutal Output: 1
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 107
 - - Input: (PolyList [-4]) 5
 - - Expected Output: -4
 - - Acutal Output: -4
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 108
 - - Input: (PolyList [0,1]) 2
 - - Expected Output: 2
 - - Acutal Output: 2
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 109
 - - Input: (PolyList [2,2]) 4
 - - Expected Output: 10 
 - - Acutal Output: 10
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 110
 - - Input: (PolyList [0,0,2]) (-4)
 - - Expected Output: 32
 - - Acutal Output: 32
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 111
 - - Input: (PolyList [2,-4,23,-2,5]) 2
 - - Expected Output: 150
 - - Acutal Output: 150
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListDegree
 - - Test Case Number: 112
 - - Input: (PolyList [])
 - - Expected Output: Undefined
 - - Acutal Output: Undefined
 - -----------------------------------------------------------------
 - - Function: polyListDegree
 - - Test Case Number: 113
 - - Input: (PolyList [2])
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: polyListDegree
 - - Test Case Number: 114
 - - Input: (PolyList [2,5,6,0,-4,6,-2])
 - - Expected Output: 6
 - - Acutal Output: 6
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListDeriv
 - - Test Case Number: 115
 - - Input: (PolyList [])
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []
 - -----------------------------------------------------------------
 - - Function: polyListDeriv
 - - Test Case Number: 116
 - - Input: (PolyList [2])
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []
 - -----------------------------------------------------------------
 - - Function: polyListDeriv
 - - Test Case Number: 117
 - - Input: (PolyList [2,-4])
 - - Expected Output: PolyList [-4]
 - - Acutal Output: PolyList [-4]
 - -----------------------------------------------------------------
 - - Function: polyListDeriv
 - - Test Case Number: 118
 - - Input: (PolyList [3,-3,0,5,7,1,-4])
 - - Expected Output: PolyList [-3,0,15,28,5,-24]
 - - Acutal Output: PolyList [-3,0,15,28,5,-24]
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 120
 - - Input: (PolyList []) (PolyList [])
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 121
 - - Input: (PolyList []) (PolyList [2])
 - - Expected Output: PolyList [2]
 - - Acutal Output: PolyList [2]
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 122
 - - Input: (PolyList [2]) (PolyList [])
 - - Expected Output: PolyList [2]
 - - Acutal Output: PolyList [2]
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 123
 - - Input: (PolyList [3]) (PolyList [3])
 - - Expected Output: PolyList [6]
 - - Acutal Output: PolyList [6]
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 124
 - - Input: (PolyList [4,5,3]) (PolyList [-2,3])
 - - Expected Output: PolyList [2,8,3]
 - - Acutal Output: PolyList [2,8,3]
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 126
 - - Input: (PolyList [-3,2,8]) (PolyList [-4,0])
 - - Expected Output: PolyList [-7,2,8]
 - - Acutal Output: PolyList [-7,2,8]
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 127
 - - Input: (PolyList [2,8]) (PolyList [-2,-8])
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 128
 - - Input: (PolyList [3,4]) (PolyList [-3,4])
 - - Expected Output: PolyList [0,8]
 - - Acutal Output: PolyList [0,8]
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 129
 - - Input: (PolyList [0,2,5,3,4,7,8]) (PolyList [0,3,-5,3,-4,-7,-8])
 - - Expected Output: PolyList [0,5,0,6]
 - - Acutal Output: PolyList [0,5,0,6]
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 130
 - - Input: (PolyList []) (PolyList [])
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 131
 - - Input: (PolyList []) (PolyList [2])
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 132
 - - Input: (PolyList [2]) (PolyList [])
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 133
 - - Input: (PolyList [3]) (PolyList [4])
 - - Expected Output: PolyList [12]
 - - Acutal Output: PolyList [12]
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 134
 - - Input: (PolyList [3]) (PolyList [4,2,-4])
 - - Expected Output: PolyList [12,6,-12]
 - - Acutal Output: PolyList [12,6,-12]
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 135
 - - Input: (PolyList [4,2,-4]) (PolyList [3])
 - - Expected Output: PolyList [12,6,-12]
 - - Acutal Output: PolyList [12,6,-12]
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 136
 - - Input: (PolyList [2,5,0,-3,-6]) (PolyList [0,4,-3,2,-4])
 - - Expected Output: PolyList [0,8,14,-11,-10,-35,12,0,24]
 - - Acutal Output: PolyList [0,8,14,-11,-10,-35,12,0,24]
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 137
 - - Input: (PolyList [])
 - - Expected Output: Coef 0
 - - Acutal Output: Coef 0
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 138
 - - Input: (PolyList [-2])
 - - Expected Output: Coef (-2)
 - - Acutal Output: Coef (-2)
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 139
 - - Input: (PolyList [0,1])
 - - Expected Output: X
 - - Acutal Output: Sum (Coef 0) (Prod (Coef 1) X)
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 140
 - - Input: (PolyList [7,0,-2])
 - - Expected Output: Sum (Prod (Coef (-2)) (Prod X X)) (Coef 7)
 - - Acutal Output: Sum (Coef 7) (Prod (Sum (Coef 0) (Prod (Coef (-2)) X)) X)
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 141
 - - Input: (PolyList [5,-2,7])
 - - Expected Output: Sum (Prod (Coef 7) (Prod X X)) (Sum (Prod (Coef (-2)) X) (Coef 5))
 - - Acutal Output: Sum (Coef 5) (Prod (Sum (Coef (-2)) (Prod (Coef 7) X)) X)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 142
 - - Input: (Coef 0)
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 143
 - - Input: (Coef 2)
 - - Expected Output: PolyList [2]
 - - Acutal Output: PolyList [2]
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 144
 - - Input: (Coef (-3))
 - - Expected Output: PolyList [-3]
 - - Acutal Output: PolyList [-3]
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 145
 - - Input: X
 - - Expected Output: PolyList [0,1]
 - - Acutal Output: PolyList [0,1]
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 146
 - - Input: (Prod (Coef 3) X)
 - - Expected Output: PolyList [0,3]
 - - Acutal Output: PolyList [0,3]
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 147
 - - Input: (Prod X X)
 - - Expected Output: PolyList [0,0,1]
 - - Acutal Output: PolyList [0,0,1]
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 148
 - - Input: (Sum (Coef 3) (Coef (-3)))
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 149
 - - Input: (Sum X X)
 - - Expected Output: PolyList [0,2]
 - - Acutal Output: PolyList [0,2]
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 150
 - - Input: (Sum (Prod (Coef 2) (Prod X X)) (Coef (-5)))
 - - Expected Output: PolyList [-5,0,2]
 - - Acutal Output: PolyList [-5,0,2]
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 151
 - - Input: (Sum (Prod X X) (Prod (Coef (-1)) (Prod X X)))
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 152
 - - Input: (Sum (Sum (Prod X X) (Prod X (Coef 3))) (Prod (Coef (-1)) (Prod X X)))
 - - Expected Output: PolyList [0,3]
 - - Acutal Output: PolyList [0,3]
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 153
 - - Input: (Prod (Sum (Coef 2) X) (Sum (Coef (-3)) X))
 - - Expected Output: PolyList [-6,-1,1]
 - - Acutal Output: PolyList [-6,-1,1]
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 154
 - - Input: (Prod (Sum (Coef 2) (Coef (-2))) (Prod (Sum (Coef 2) X) (Sum (Coef (-3)) X)))
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []
 - -----------------------------------------------------------------
 End of test cases
 -}
