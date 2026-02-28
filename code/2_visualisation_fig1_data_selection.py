import argparse
import os
import re

import geopandas as gpd
import matplotlib.pyplot as plt
import pandas as pd
import yaml
from mpl_toolkits.basemap import Basemap
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
from pandas.tseries.offsets import DateOffset


class Figure1Plotter:
    def __init__(self):
        self.cities = [
            "Beijing",
            "Guangzhou",
            "Wuhan",
            "Xian",
            "Lanzhou",
            "Suzhou",
            "Wenzhou",
            "Yunfu",
        ]

        self.script_dir = os.path.dirname(os.path.abspath(__file__))
        self.repo_root = os.path.abspath(os.path.join(self.script_dir, os.pardir))
        self.flu_path = os.path.join(self.repo_root, "data", "processed", "flu")
        self.rsv_path = os.path.join(self.repo_root, "data", "processed", "rsv")
        self.figures_path = os.path.join(self.repo_root, "figures", "fig 1")
        self.config_path = os.path.join(self.repo_root, "code", "fig1_plot_config.yaml")

        if not os.path.isdir(self.figures_path):
            os.makedirs(self.figures_path, exist_ok=True)

        self.geojson_path_province = os.path.join(
            self.repo_root, "data", "external", "basemap-province.geojson"
        )
        self.geojson_path_city = os.path.join(
            self.repo_root, "data", "external", "basemap-city.geojson"
        )
        self.geojson_path_outline = os.path.join(
            self.repo_root, "data", "external", "outline.geojson"
        )

        self.highlight_cities = [
            "北京市",
            "广州市",
            "武汉市",
            "西安市",
            "兰州市",
            "苏州市",
            "温州市",
            "云浮市",
        ]

        self.config = self.load_config()

        plt.rcParams["font.family"] = "Times New Roman"
        plt.rcParams["font.size"] = 22

    def load_config(self):
        with open(self.config_path, "r", encoding="utf-8") as handle:
            config = yaml.safe_load(handle) or {}

        defaults = {
            "basemap_figsize": [16, 12],
            "sequence_figsize": [9, 6],
            "ticklabel_size": 24,
            "sequence_title_size": 36,
            "highlight_color": "tab:cyan",
            "flu_color": "tab:green",
            "rsv_color": "tab:orange",
            "sequence_bottom_margin": 0.18,
            "sequence_tick_rotation": 0,
            "sequence_left_margin": 0.18,
            "sequence_right_margin": 0.97,
            "sequence_top_margin": 0.92,
            "basemap_left_margin": 0.08,
            "basemap_bottom_margin": 0.08,
            "basemap_right_margin": 0.98,
            "basemap_top_margin": 0.98,
            "legend_figsize": [6, 2],
            "legend_fontsize": 24,
            "legend_linewidth": 2,
        }
        for key, value in defaults.items():
            config.setdefault(key, value)
        return config

    def find_city_file(self, city, directory):
        pattern = re.compile(rf"^[^-]+-[^-]+-{city}-(Cases|Rates)\.csv$")
        matches = [f for f in os.listdir(directory) if pattern.match(f)]
        if not matches:
            return None
        cases = [f for f in matches if f.endswith("-Cases.csv")]
        filename = cases[0] if cases else matches[0]
        return os.path.join(directory, filename)

    def coerce_date_column(self, df):
        if "date" in df.columns:
            date_col = "date"
        elif "Year-Month" in df.columns:
            date_col = "Year-Month"
        else:
            raise ValueError("No date column found.")

        parsed = pd.to_datetime(df[date_col], errors="coerce")
        if parsed.isna().all():
            parsed = pd.to_datetime(df[date_col], format="%b-%y", errors="coerce")
        if parsed.isna().all():
            parsed = pd.to_datetime(df[date_col], format="%Y-%m", errors="coerce")
        if parsed.isna().all():
            raise ValueError("Unsupported date format.")

        df = df.copy()
        df["date"] = parsed
        return df

    def coerce_value_column(self, df):
        if "value" in df.columns:
            value_col = "value"
        elif "Positive Cases" in df.columns:
            value_col = "Positive Cases"
        else:
            raise ValueError("No value column found.")

        df = df.copy()
        df["value"] = df[value_col]
        return df

    def plot_city_sequence(self, city):
        flu_file = self.find_city_file(city, self.flu_path)
        rsv_file = self.find_city_file(city, self.rsv_path)

        if not flu_file or not rsv_file:
            print(f"Data for {city} not found in one of the directories.")
            return

        flu_df = pd.read_csv(flu_file)
        rsv_df = pd.read_csv(rsv_file)

        try:
            flu_df = self.coerce_value_column(self.coerce_date_column(flu_df))
            rsv_df = self.coerce_value_column(self.coerce_date_column(rsv_df))
        except ValueError as exc:
            print(f"Skipping {city} due to parsing issues: {exc}")
            return

        combined_dates = pd.to_datetime(
            sorted(set(flu_df["date"]).union(set(rsv_df["date"])))
        )
        extended_start = combined_dates.min() - DateOffset(months=6)
        extended_end = combined_dates.max() + DateOffset(months=6)

        min_year = extended_start.year
        max_year = extended_end.year
        tick_step = 3
        tick_years = list(range(min_year, max_year + 1, tick_step))
        tick_dates = [pd.Timestamp(year=year, month=1, day=1) for year in tick_years]

        fig, ax1 = plt.subplots(figsize=tuple(self.config["sequence_figsize"]))

        ax1.plot(
            flu_df["date"],
            flu_df["value"],
            color=self.config["flu_color"],
            label="Flu",
            linewidth=2,
            marker="o",
        )
        # ax1.set_ylabel("Positive Cases (Flu)", color=self.config["flu_color"])
        ax1.tick_params(axis="y", labelcolor=self.config["flu_color"])

        ax2 = ax1.twinx()
        ax2.plot(
            rsv_df["date"],
            rsv_df["value"],
            color=self.config["rsv_color"],
            label="RSV",
            linewidth=2,
            marker="s",
        )
        # ax2.set_ylabel(
        #     "Positive Cases (RSV)",
        #     color=self.config["rsv_color"],
        #     rotation=270,
        #     labelpad=20,
        # )
        ax2.tick_params(axis="y", labelcolor=self.config["rsv_color"])

        ax1.set_xticks(tick_dates)
        ax1.set_xticklabels(
            [date.strftime("%Y") for date in tick_dates],
            rotation=self.config["sequence_tick_rotation"],
            ha="center",
        )

        ax1.grid(True, linestyle="-.", color="lightgrey", axis="both")
        ax1.tick_params(axis="x", labelsize=self.config["ticklabel_size"])
        ax1.tick_params(axis="y", labelsize=self.config["ticklabel_size"])
        ax2.tick_params(axis="y", labelsize=self.config["ticklabel_size"])
        plt.title(f"{city}", fontsize=self.config["sequence_title_size"])

        fig.subplots_adjust(
            left=self.config["sequence_left_margin"],
            right=self.config["sequence_right_margin"],
            top=self.config["sequence_top_margin"],
            bottom=self.config["sequence_bottom_margin"],
        )
        plt.savefig(
            os.path.join(self.figures_path, f"{city}_disease_sequence.png"),
            dpi=300,
        )
        plt.close()

    def plot_sequences(self):
        for city in self.cities:
            self.plot_city_sequence(city)
        print("Disease sequence plots saved.")

    def plot_basemap(self):
        if not (
            os.path.isfile(self.geojson_path_province)
            and os.path.isfile(self.geojson_path_city)
            and os.path.isfile(self.geojson_path_outline)
        ):
            print("Basemap geojson files not found under data/external/.")
            return

        gdf_province = gpd.read_file(self.geojson_path_province)
        gdf_city = gpd.read_file(self.geojson_path_city)
        gdf_outline = gpd.read_file(self.geojson_path_outline)

        fig, ax = plt.subplots(figsize=tuple(self.config["basemap_figsize"]))
        plt.tight_layout()

        m = Basemap(
            projection="lcc",
            llcrnrlon=78,
            llcrnrlat=12.5,
            urcrnrlon=148,
            urcrnrlat=53,
            lat_1=20.0,
            lat_2=40.0,
            lon_0=105.0,
            resolution="i",
            area_thresh=1000.0,
            ax=ax,
        )
        m.drawparallels(
            range(20, 60, 10),
            labels=[1, 0, 0, 0],
            color="gray",
            linewidth=0.6,
            zorder=0,
            fontsize=self.config["ticklabel_size"],
            dashes=[5, 5],
        )
        m.drawmeridians(
            range(70, 160, 10),
            labels=[0, 0, 0, 1],
            color="gray",
            linewidth=0.6,
            zorder=0,
            fontsize=self.config["ticklabel_size"],
            dashes=[5, 5],
        )

        for _, row in gdf_province.iterrows():
            geom = row["geometry"]
            if geom.geom_type == "Polygon":
                coords = pd.DataFrame(geom.exterior.coords)
                x, y = m(coords[0].to_numpy(), coords[1].to_numpy())
                ax.add_patch(
                    plt.Polygon(
                        list(zip(x, y)),
                        facecolor="none",
                        edgecolor="gray",
                        linewidth=0.6,
                        zorder=11,
                    )
                )
            elif geom.geom_type == "MultiPolygon":
                for poly in geom.geoms:
                    coords = pd.DataFrame(poly.exterior.coords)
                    x, y = m(coords[0].to_numpy(), coords[1].to_numpy())
                    ax.add_patch(
                        plt.Polygon(
                            list(zip(x, y)),
                            facecolor="none",
                            edgecolor="gray",
                            linewidth=0.6,
                            zorder=11,
                        )
                    )

        for _, row in gdf_city.iterrows():
            geom = row["geometry"]
            city_name = row["市"]
            facecolor = (
                self.config["highlight_color"]
                if city_name in self.highlight_cities
                else "white"
            )
            edgecolor = "black" if city_name in self.highlight_cities else "lightgray"
            linewidth = 0.6 if city_name in self.highlight_cities else 0.4
            zorder = 14 if city_name in self.highlight_cities else 10

            if geom.geom_type == "Polygon":
                coords = pd.DataFrame(geom.exterior.coords)
                x, y = m(coords[0].to_numpy(), coords[1].to_numpy())
                ax.add_patch(
                    plt.Polygon(
                        list(zip(x, y)),
                        facecolor=facecolor,
                        edgecolor=edgecolor,
                        linewidth=linewidth,
                        zorder=zorder,
                    )
                )
            elif geom.geom_type == "MultiPolygon":
                for poly in geom.geoms:
                    coords = pd.DataFrame(poly.exterior.coords)
                    x, y = m(coords[0].to_numpy(), coords[1].to_numpy())
                    ax.add_patch(
                        plt.Polygon(
                            list(zip(x, y)),
                            facecolor=facecolor,
                            edgecolor=edgecolor,
                            linewidth=linewidth,
                            zorder=zorder,
                        )
                    )

        for _, row in gdf_outline.iterrows():
            geom = row["geometry"]
            if geom.geom_type == "Polygon":
                coords = pd.DataFrame(geom.exterior.coords)
                x, y = m(coords[0].to_numpy(), coords[1].to_numpy())
                ax.add_patch(
                    plt.Polygon(
                        list(zip(x, y)),
                        facecolor="none",
                        edgecolor="black",
                        linewidth=1.5,
                        zorder=12,
                    )
                )
            elif geom.geom_type == "MultiPolygon":
                for poly in geom.geoms:
                    coords = pd.DataFrame(poly.exterior.coords)
                    x, y = m(coords[0].to_numpy(), coords[1].to_numpy())
                    ax.add_patch(
                        plt.Polygon(
                            list(zip(x, y)),
                            facecolor="none",
                            edgecolor="black",
                            linewidth=1.5,
                            zorder=12,
                        )
                    )

        for spine in ax.spines.values():
            spine.set_linewidth(1.5)

        left, bottom, width, height = 0.77, 0.09, 0.20, 0.25
        ax2 = fig.add_axes([left, bottom, width, height])
        gdf_province.plot(ax=ax2, facecolor="none", edgecolor="black", linewidth=0.8)
        gdf_outline.plot(ax=ax2, facecolor="none", edgecolor="black", linewidth=0.8)
        ax2.set_xlim(106, 122)
        ax2.set_ylim(0, 20)
        ax2.grid(ls="-.", lw=0.2, color="lightgray")
        ax2.tick_params(
            labelbottom=False, labeltop=False, labelleft=False, labelright=False
        )

        fig.subplots_adjust(
            left=self.config["basemap_left_margin"],
            right=self.config["basemap_right_margin"],
            top=self.config["basemap_top_margin"],
            bottom=self.config["basemap_bottom_margin"],
        )
        plt.savefig(os.path.join(self.figures_path, "basemap.png"), dpi=600)
        plt.show()

    def plot_legend(self):
        fig, ax = plt.subplots(figsize=tuple(self.config["legend_figsize"]))
        ax.axis("off")

        handles = [
            Patch(
                facecolor=self.config["highlight_color"],
                edgecolor="black",
                label="Cities identified",
            ),
            Line2D(
                [0],
                [0],
                color=self.config["rsv_color"],
                marker="s",
                linewidth=self.config["legend_linewidth"],
                label="RSV",
            ),
            Line2D(
                [0],
                [0],
                color=self.config["flu_color"],
                marker="o",
                linewidth=self.config["legend_linewidth"],
                label="INFV",
            ),
        ]

        ax.legend(
            handles=handles,
            loc="center",
            ncol=1,
            frameon=False,
            fontsize=self.config["legend_fontsize"],
        )

        plt.savefig(os.path.join(self.figures_path, "legend.png"), dpi=300)
        plt.close()


def parse_args():
    parser = argparse.ArgumentParser(description="Figure 1 plots")
    parser.add_argument(
        "--plot",
        choices=["sequence", "basemap", "legend", "all"],
        default="all",
        help="Choose which plot to generate.",
    )
    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    plotter = Figure1Plotter()
    if args.plot in {"sequence", "all"}:
        plotter.plot_sequences()
    if args.plot in {"basemap", "all"}:
        plotter.plot_basemap()
    if args.plot in {"legend", "all"}:
        plotter.plot_legend()

