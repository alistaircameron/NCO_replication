from scipy.stats import ttest_ind_from_stats as ttest
import numpy as np
import pandas as pd
import re, os, time


def trailing_zeros(x, dp=2):
    """This function works much better than 'rounder' below. which is just kept here for use with the charity (baseline) treatment..."""
    x = f"{float(x):.{dp}f}"
    return x


def rounder(x, n=2):
    """Quick and dirty function to round Stata's strings"""
    try:
        x = round(float(x), n)
    except:
        pass
    return x


def brackets(x):
    """add brackets around the standard errors"""
    try:
        x = "(" + str(x) + ")"
    except:
        pass
    return x


def mde(x):
    """gross function to return the minimum detectable effect - see Haushofer & Shapiro 2016"""
    try:
        x = re.sub(
            "[^0-9^.]", "", str(x)
        )  # KEEP just the numbers and the decimal place.
        x = round(float(x) * 2.8, 2)

    except:
        x = "error"
    return x


# Reduced, treatment effects


def rte(part=1, give=True, ext=True, reduced=True):

    treatments = ["Anarchy", "Tax", "UBI", "Effort", "Luck"]
    countries = [
        "\midrule \\\ Germany",
        "\midrule \\\ India",
        "\midrule \\\ Indonesia",
        "\midrule \\\ USA",
    ]
    ncols = 5

    if give == True:
        gt = "give"
    else:
        gt = "take"

    if ext == True:
        ext_int = ""
    else:
        ext_int = "_int"

    red = "_reduced_"
    if reduced != True:
        red = "_full_"
        ncols = 21

    fname = gt + ext_int + red + str(part) + ".raw"

    # Nb. this is: coeff stderr pval
    with open(fname) as file:
        output = [f.strip() for f in file]

    output = output[0:-1]  # kill the last element

    for i in range(len(output)):
        output[i] = [x for x in output[i].split(" ") if x != ""]

    for i in range(len(output)):
        try:
            pval = float(output[i][2])
            output[i] = [str(trailing_zeros(float(output[i][0]))), float(output[i][1])]

            # Add some significance stars.
            if 0.05 < pval <= 0.1:
                output[i][0] += "*"
            elif 0.01 < pval <= 0.05:
                output[i][0] += "**"
            elif pval <= 0.01:
                output[i][0] += "***"

            output[i][1] = brackets(trailing_zeros(output[i][1]))

        except:
            # For the base group, treat them different
            output[i] = [
                trailing_zeros(float(output[i][0])),
                brackets(trailing_zeros(output[i][1])),
            ]

    df = pd.DataFrame(index=range(5 * 4 * 2), columns=range(ncols))

    # First two columns - titles
    j = 0
    k = 0
    for i in range(df.shape[0]):
        if i % 10 == 0:
            df.iloc[i, 0] = countries[j]
            j += 1
        else:
            df.iloc[i, 0] = ""

        if i % 2 == 0:
            df.iloc[i, 1] = treatments[k]
            k += 1
            if k == 5:
                k = 0
        else:
            df.iloc[i, 1] = ""

    # Remaining columns - data.
    k = 0
    for j in range(2, df.shape[1]):
        for i in range(df.shape[0]):
            if i % 2 == 0:
                df.iloc[i, j] = output[k][0]
                df.iloc[i + 1, j] = output[k][1]
                k += 1

    kill_off = [x + 1 for x in range(len(df)) if df.loc[x, 1] == "Anarchy"]
    df = df[~df.index.isin(kill_off)]

    return df
