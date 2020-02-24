def count_change(coins, num_coins, value):
    if (value == 0):
        return 1

    elif (value < 0):
        return 0

    else:
        if (num_coins <= 0):
            return 0
        else:
            return count_change(coins, num_coins - 1, value) + count_change(coins, num_coins, value - coins[num_coins-1])


if __name__ == '__main__':
    print(count_change([1, 5, 10, 25, 50], 5, 100))
