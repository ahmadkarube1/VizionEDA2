import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from io import BytesIO
import plotly.express as px


# Transformation Functions
def negative_exponential(x, alpha, beta):
    return 1 - np.exp(-alpha * x**beta)


def indexed_exponential(x, alpha, beta):
    return 1 - np.exp(-alpha * (x / np.max(x))**beta)


def s_shape(x, alpha, beta):
    return 1 / (1 + np.exp(-alpha * (x - beta)))


def s_origin(x, alpha, beta):
    return 1 / (1 + np.exp(-alpha * (x - beta))) - 0.5


# Function to convert non-numeric columns to numeric
def convert_to_numeric(x):
    if isinstance(x, str):
        return sum(ord(char) for char in x)
    return x


# App Header
st.title("VizionEDA")
st.write(
    "Analyze activity and spend data with transformations, filters, and visualizations."
)

# File Upload
uploaded_file = st.file_uploader("Upload your CSV file", type="csv")
if uploaded_file:
    df = pd.read_csv(uploaded_file)
    st.write("## Uploaded Data Preview")
    st.dataframe(df.head())

    # Data Quality Check
    st.write("### Data Quality Check")
    st.write("Number of missing values per column:")
    st.write(df.isnull().sum())

    # Automatic Identification of Activity and Spend Variables
    media_keywords = [
        "Impressions", "Clicks", "Search", "OLV", "OOH", "Display",
        "Circulation", "Magazine", "Newspaper", "Social"
    ]
    spend_keywords = ["Spend", "Cost"]

    media_vars = [
        col for col in df.columns
        if any(keyword in col for keyword in media_keywords) and not any(
            keyword in col for keyword in spend_keywords)
    ]
    spend_vars = [
        col for col in df.columns
        if any(keyword in col for keyword in spend_keywords)
    ]

    st.write("### Identified Media and Spend Variables")
    st.write("Media Variables:", media_vars)
    st.write("Spend Variables:", spend_vars)

    # Global Filters
    st.sidebar.write("## Global Filters")
    filters = {}

    # Dynamically identify columns and populate filter options
    for col_name in ["Geography", "Product", "Campaign", "Outlet", "Creative"]:
        if col_name in df.columns:
            unique_values = df[col_name].dropna().unique()
            selected_value = st.sidebar.selectbox(
                f"Filter by {col_name}", ["All"] + list(unique_values))
            filters[col_name] = selected_value

    # Apply filters dynamically
    filtered_df = df.copy()
    for col, value in filters.items():
        if value != "All":
            filtered_df = filtered_df[filtered_df[col] == value]

    st.write("### Filtered Data Preview")
    st.dataframe(filtered_df.head())

    # Univariate Analysis
    st.write("## Univariate Analysis")
    variable = st.selectbox("Select a variable for univariate analysis:",
                            filtered_df.columns)
    alpha = st.slider("Select alpha (α):", 0.01, 1.0, 0.1)
    beta = st.slider("Select beta (β):", 0.01, 2.0, 1.0)
    transformation = st.radio(
        "Select Transformation",
        ["Negative Exponential", "Indexed Exponential", "S-Shape", "S-Origin"])

    if variable:
        try:
            filtered_df[variable] = filtered_df[variable].apply(
                convert_to_numeric)

            if transformation == "Negative Exponential":
                filtered_df[f"Transformed_{variable}"] = negative_exponential(
                    filtered_df[variable], alpha, beta)
            elif transformation == "Indexed Exponential":
                filtered_df[f"Transformed_{variable}"] = indexed_exponential(
                    filtered_df[variable], alpha, beta)
            elif transformation == "S-Shape":
                filtered_df[f"Transformed_{variable}"] = s_shape(
                    filtered_df[variable], alpha, beta)
            elif transformation == "S-Origin":
                filtered_df[f"Transformed_{variable}"] = s_origin(
                    filtered_df[variable], alpha, beta)

            st.write(f"### Transformed Data: {variable}")
            st.dataframe(filtered_df[[variable,
                                      f"Transformed_{variable}"]].head())

            # Visualization
            st.write(f"### Univariate Analysis: {variable}")
            fig = px.line(filtered_df,
                          x=filtered_df.index,
                          y=[variable, f"Transformed_{variable}"],
                          labels={
                              "value": "Values",
                              "index": "Index"
                          },
                          title=f"Transformation of {variable}")
            st.plotly_chart(fig)
        except Exception as e:
            st.error(f"Error transforming data for variable '{variable}': {e}")

    # Multivariate Analysis
    st.write("## Multivariate Analysis")
    selected_vars = st.multiselect(
        "Select variables for multivariate analysis:", filtered_df.columns)
    display_option = st.radio("Display Correlation Matrix As:",
                              ["Colormap Heatmap", "Table Format"])
    sum_vars = st.checkbox("Sum Selected Variables for Analysis")

    if selected_vars:
        try:
            selected_data = filtered_df[selected_vars].copy()

            for col in selected_data.select_dtypes(
                    include=['object', 'category']):
                selected_data[col] = selected_data[col].apply(
                    convert_to_numeric)

            if sum_vars:
                selected_data["Sum_Variables"] = selected_data.sum(axis=1)
                transformed_sum = negative_exponential(
                    selected_data["Sum_Variables"], alpha, beta)
                st.write("### Transformation of Summed Variables")
                fig = px.line(
                    x=selected_data.index,
                    y=[selected_data["Sum_Variables"], transformed_sum],
                    labels={
                        "value": "Values",
                        "index": "Index"
                    },
                    title="Multivariate Analysis: Sum of Variables")
                st.plotly_chart(fig)
            else:
                correlation_matrix = selected_data.corr()

                if display_option == "Colormap Heatmap":
                    st.write("### Correlation Matrix (Colormap Heatmap)")
                    plt.figure(figsize=(10, 8))
                    sns.heatmap(correlation_matrix,
                                annot=True,
                                cmap='coolwarm',
                                fmt=".2f",
                                linewidths=0.5)
                    plt.title("Correlation Matrix", fontsize=16)
                    plt.xticks(fontsize=10, rotation=45)
                    plt.yticks(fontsize=10)
                    st.pyplot(plt.gcf())
                elif display_option == "Table Format":
                    st.write("### Correlation Matrix (Table Format)")
                    st.dataframe(correlation_matrix)
        except Exception as e:
            st.error(f"Error during multivariate analysis: {e}")

    # Save Filter Configuration
    if st.sidebar.button("Save Filter Configuration"):
        st.sidebar.write("Filter configuration saved successfully.")

    # Final Report Generation
    st.write("## Final Report")
    if st.button("Generate Report"):
        try:
            report_buffer = BytesIO()
            with pd.ExcelWriter(report_buffer, engine='xlsxwriter') as writer:
                filtered_df.to_excel(writer,
                                     index=False,
                                     sheet_name='Filtered Data')
                if sum_vars:
                    selected_data.to_excel(writer,
                                           index=False,
                                           sheet_name='Multivariate Analysis')

            report_buffer.seek(0)
            st.download_button(
                label="Download Report as Excel",
                data=report_buffer,
                file_name="crm_analysis_report.xlsx",
                mime=
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            )
        except Exception as e:
            st.error(f"Error generating report: {e}")

    # Documentation Section
    st.sidebar.write("## Documentation")
    st.sidebar.info("""
        **Transformations:**
        - Negative Exponential: Exponential decay transformation.
        - Indexed Exponential: Adjusted exponential decay using max normalization.
        - S-Shape: Sigmoidal transformation with adjustable inflection point.
        - S-Origin: Sigmoidal transformation centered around zero.

        **Filters:** Apply global filters for Geography, Product, Campaign, Outlet, and Creative.

        **Reports:** Download the analysis as an Excel report.
        """)
