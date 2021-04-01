def check(mid, array, n, K): 
    count = 0
    sum = 0
    for i in range(n): 
        if (array[i] > mid): 
            return False
        sum += array[i] 
        if (sum > mid): 
            count += 1
            sum = array[i] 
    count += 1
    if (count <= K): 
        return True
    return False
 
def solve(array, n, K): 
    start = 1
    end = 0
    for i in range(n): 
        end += array[i] 
    answer = 0
    
    while (start <= end): 
        mid = (start + end) // 2
        if (check(mid, array, n, K)): 
            answer = mid 
            end = mid - 1
        else: 
            start = mid + 1
    return answer 
n, K = map(int, input().split())
array = list(map(int, input().split()))
print(solve(array, n, K)) 
