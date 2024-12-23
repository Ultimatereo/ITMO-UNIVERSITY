import typing

import pytest

from .banner_engine import (
    BannerStat, Banner
)

TEST_DEFAULT_CTR = 0.1


@pytest.fixture(scope="function")
def test_banners() -> list[Banner]:
    return [
        Banner("b1", cost=1, stat=BannerStat(10, 20)),
        Banner("b2", cost=250, stat=BannerStat(20, 20)),
        Banner("b3", cost=100, stat=BannerStat(0, 20)),
        Banner("b4", cost=100, stat=BannerStat(1, 20)),
    ]


@pytest.mark.parametrize("clicks, shows, expected_ctr", [(1, 1, 1.0), (20, 100, 0.2), (5, 100, 0.05)])
def test_banner_stat_ctr_value(clicks: int, shows: int, expected_ctr: float) -> None:
    pass


def test_empty_stat_compute_ctr_returns_default_ctr() -> None:
    pass


def test_banner_stat_add_show_lowers_ctr() -> None:
    pass


def test_banner_stat_add_click_increases_ctr() -> None:
    pass


def test_get_banner_with_highest_cpc_returns_banner_with_highest_cpc(test_banners: list[Banner]) -> None:
    pass


def test_banner_engine_raise_empty_storage_exception_if_constructed_with_empty_storage() -> None:
    pass


def test_engine_send_click_not_fails_on_unknown_banner(test_banners: list[Banner]) -> None:
    pass


def test_engine_with_zero_random_probability_shows_banner_with_highest_cpc(test_banners: list[Banner]) -> None:
    pass


@pytest.mark.parametrize("expected_random_banner", ["b1", "b2", "b3", "b4"])
def test_engine_with_1_random_banner_probability_gets_random_banner(
        expected_random_banner: str,
        test_banners: list[Banner],
        monkeypatch: typing.Any
        ) -> None:
    pass


def test_total_cost_equals_to_cost_of_clicked_banners(test_banners: list[Banner]) -> None:
    pass


def test_engine_show_increases_banner_show_stat(test_banners: list[Banner]) -> None:
    pass


def test_engine_click_increases_banner_click_stat(test_banners: list[Banner]) -> None:
    pass
