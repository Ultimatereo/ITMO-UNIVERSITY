def find_value(nums: list[int] | range, value: int) -> bool:
    """
    Find value in sorted sequence
    :param nums: sequence of integers. Could be empty
    :param value: integer to find
    :return: True if value exists, False otherwise
    """
    start = 0
    end = len(nums) - 1
    while end - start > 0:
        mid = (start + end) // 2
        if nums[mid] > value:
            end = mid - 1
        elif nums[mid] < value:
            start = mid + 1
        else:
            return True
    return len(nums) > 0 and nums[start] == value
