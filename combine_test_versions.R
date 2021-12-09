input 1: each column is a question from exam A
input 2: each column is a question from exam B
input 3: df like below, id column optional

A  |  B  | id        | group
1  |  1  | CLT
2  |     | viz
3  |  4
...
30 |  30
   |  3

output:

  - $response_data - df with columns like A1_B1, A0_B3, A2_B0, A3_B4
  - $meta_data - df like below, status column assumes you go from A to B

    question_label | id | group | A | B | test_a_name | test_b_name | status
    A1_B1          |    |       | 1 | 1 | Question1   | Ques1       | unchanged, added, removed, moved

test_combined <- combine_test_versions(test_a = ___, test_b = ___, mapping = ___)

  - check1: parse column headers of A and B to make sure numbers are all accounted for in mapping input

test_combined$response_data
test_combined$meta_data



# simulated data for testing the function

test_a: 15 x 10
test_b: 20 x 10

4 questions from A unchanged
3 moved
3 remove

3 questions added to B
