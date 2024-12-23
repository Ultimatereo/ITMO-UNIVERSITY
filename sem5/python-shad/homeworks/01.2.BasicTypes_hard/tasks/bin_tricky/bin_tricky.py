from collections.abc import Sequence


def find_median(nums1: Sequence[int], nums2: Sequence[int]) -> float:
    """
    Find median of two sorted sequences. At least one of sequences should be not empty.
    :param nums1: sorted sequence of integers
    :param nums2: sorted sequence of integers
    :return: middle value if sum of sequences' lengths is odd
             average of two middle values if sum of sequences' lengths is even
    """
    n = len(nums1)
    m = len(nums2)
    if n > m:
        return find_median(nums2, nums1)
    start = 0
    end = n
    real_mid = (n + m) // 2
    while end - start >= 0:
        mid = (start + end) // 2
        ind1 = mid
        ind2 = real_mid - mid
        left1, left2 = float('-inf'), float('-inf')
        right1, right2 = float('inf'), float('inf')
        if 0 <= ind1 - 1 < n:
            left1 = nums1[ind1 - 1]
        if 0 <= ind2 - 1 < m:
            left2 = nums2[ind2 - 1]
        if 0 <= ind1 < n:
            right1 = nums1[ind1]
        if 0 <= ind2 < m:
            right2 = nums2[ind2]

        if left1 <= right2 and left2 <= right1:
            before = ind1 + ind2 - 2
            s = [min(left1, left2), max(left1, left2), min(right1, right2), max(right1, right2)]
            if (m + n) % 2 == 0:
                return (s[real_mid - before] + s[real_mid - before - 1]) / 2.0
            return float(s[real_mid - before])
        elif left1 > right2:
            end = mid - 1
        else:
            start = mid + 1
    assert False, "Not reachable"
