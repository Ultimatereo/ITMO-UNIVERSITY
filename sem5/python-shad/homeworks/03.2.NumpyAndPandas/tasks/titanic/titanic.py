import typing as tp

import pandas as pd


def male_age(titanic: pd.DataFrame) -> float:
    """
    Return mean age of survived men, embarked in Southampton with fare > 30
    :param titanic: dataframe
    :return: mean age
    """
    filtered_data = titanic[(titanic['Survived'] == 1) & (titanic['Sex'] == 'male') & (titanic['Embarked'] == 'S')
                            & (titanic['Fare'] > 30)]
    filtered_data = filtered_data.dropna(subset=['Age'])
    return filtered_data['Age'].mean()


def nan_columns(titanic: pd.DataFrame) -> tp.Iterable[str]:
    """
    Return list of columns containing nans
    :param titanic: dataframe
    :return: series of columns
    """
    return titanic.columns[titanic.isnull().any()].tolist()


def class_distribution(titanic: pd.DataFrame) -> pd.Series:
    """
    Return Pclass distrubution
    :param titanic: dataframe
    :return: series with ratios
    """
    class_proportions = titanic['Pclass'].value_counts(normalize=True).sort_index()
    return class_proportions


def families_count(titanic: pd.DataFrame, k: int) -> int:
    """
    Compute number of families with more than k members
    :param titanic: dataframe,
    :param k: number of members,
    :return: number of families
    """
    titanic['LastName'] = titanic['Name'].str.split(',').str[0]
    family_counts = titanic['LastName'].value_counts()
    return (family_counts > k).sum()


def mean_price(titanic: pd.DataFrame, tickets: tp.Iterable[str]) -> float:
    """
    Return mean price for specific tickets list
    :param titanic: dataframe,
    :param tickets: list of tickets,
    :return: mean fare for this tickets
    """
    filtered_data = titanic[titanic['Ticket'].isin(tickets)]
    return filtered_data['Fare'].mean()


def max_size_group(titanic: pd.DataFrame, columns: list[str]) -> tp.Iterable[tp.Any]:
    """
    For given set of columns compute most common combination of values of these columns
    :param titanic: dataframe,
    :param columns: columns for grouping,
    :return: list of most common combination
    """
    most_common_combination = titanic.groupby(columns).size().idxmax()
    return most_common_combination


def dead_lucky(titanic: pd.DataFrame) -> float:
    """
    Compute dead ratio of passengers with lucky tickets.
    A ticket is considered lucky when it contains an even number of digits in it
    and the sum of the first half of digits equals the sum of the second part of digits
    ex:
    lucky: 123222, 2671, 935755
    not lucky: 123456, 62869, 568290
    :param titanic: dataframe,
    :return: ratio of dead lucky passengers
    """

    def is_lucky(ticket_number):  # type: ignore
        try:
            ticket_number = int(ticket_number)
            ticket_str = str(ticket_number)
            if len(ticket_str) % 2 != 0:
                return False
            half_length = len(ticket_str) // 2
            first_half_sum = sum(map(int, ticket_str[:half_length]))
            second_half_sum = sum(map(int, ticket_str[half_length:]))
            return first_half_sum == second_half_sum
        except ValueError:
            return Falsel


    return 1 - titanic[titanic['Ticket'].apply(is_lucky)]['Survived'].mean()
