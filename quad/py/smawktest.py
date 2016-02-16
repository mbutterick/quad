import SMAWK

m1 = [
[25, 42, 57, 78, 90, 103, 123, 142, 151],
[21, 35, 48, 65, 76, 85, 105, 123, 130],
[13, 26, 35, 51, 58, 67, 86, 100, 104],
[10, 20, 28, 42, 48, 56, 75, 86, 88],
[20, 29, 33, 44, 49, 55, 73, 82, 80],
[13, 21, 24, 35, 39, 44, 59, 65, 59],
[19, 25, 28, 38, 42, 44, 57, 61, 52],
[35, 37, 40, 48, 48, 49, 62, 62, 49],
[37, 36, 37, 42, 39, 39, 51, 50, 37],
[41, 39, 37, 42, 35, 33, 44, 43, 29],
[58, 56, 54, 55, 47, 41, 50, 47, 29],
[66, 64, 61, 61, 51, 44, 52, 45, 24],
[82, 76, 72, 70, 56, 49, 55, 46, 23],
[99, 91, 83, 80, 63, 56, 59, 46, 20],
[124, 116, 107, 100, 80, 71, 72, 58, 28],
[133, 125, 113, 106, 86, 75, 74, 59, 25],
[156, 146, 131, 120, 97, 84, 80, 65, 31],
[178, 164, 146, 135, 110, 96, 92, 73, 39]]


num_rows = len(m1)
row_indices = range(num_rows)
num_cols = len(m1[0])
col_indices = range(num_cols)


def matrix_func1(i, j):
    try:
        return m1[i][j]
    except:
        return -1 * i

def matrix_func2(i, j):
    try:
        return m1[j][i]
    except:
        return -1 * i


cm_hash = SMAWK.ConcaveMinima(row_indices, col_indices, matrix_func1)
cm_hash2 = SMAWK.ConcaveMinima(col_indices, row_indices, matrix_func2)

print cm_hash2

'''
{0: (10, 3), 1: (20, 3), 2: (24, 5), 3: (35, 5), 4: (35, 9), 5: (33, 9), 6: (44, 9), 7: (43, 9), 8: (20, 13)}
'''
 
ocm = SMAWK.OnlineConcaveMinima(matrix_func1, 0)
ocm2 = SMAWK.OnlineConcaveMinima(matrix_func2, 0)

ocm_hash = dict()
for j in col_indices:
    ocm_hash[j] = (ocm.value(j), ocm.index(j))

ocm_hash2 = dict()
for j in row_indices:
    ocm_hash2[j] = (ocm2.value(j), ocm2.index(j))

print ocm_hash2

'''
{0: (0, None), 1: (42, 0), 2: (48, 1), 3: (51, 2), 4: (48, 3), 5: (55, 4), 6: (59, 5), 7: (61, 6), 8: (49, 7)}'''