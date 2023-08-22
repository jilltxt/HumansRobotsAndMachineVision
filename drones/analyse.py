import pandas as pd
import matplotlib.pyplot as plt


# Load the CSV file into a DataFrame
creativeworks = pd.read_csv(
    "https://raw.githubusercontent.com/jilltxt/HumansRobotsAndMachineVision/main/data/creativeworks.csv",
    dtype={12: str},
)


def extract_tech(technology):
    """
    Create a new dataframe from creativeworks containing only works with a specific technology.

     Parameters
     ----------
     technology : list[str]
         A value in TechRef or TechUsed, with 'quote' marks around it.

     Returns
     -------
     Pandas dataframe named the same as the parameter.

     Examples
     --------
     >>> exctract_tech('Drones')

     >>> extract_tech('Facial recognition')

    """
    technology = creativeworks[
        creativeworks["TechRef"].str.contains(technology, case=False, na=False)
        | creativeworks["TechUsed"].str.contains(technology, case=False, na=False)
    ]
    technology = technology.drop_duplicates()
    return technology


# 2. Defining the analyse_aspect function
def analyse_aspect(technology, feature_name):
    """All-in-one function to filter data and create a pie chart

    Args:
        technology (_type_): _description_
        feature_name (_type_): _description_
    """    
    # a) Select WorkTitle and feature_name and (rows where technology is in TechRef or TechUsed) and remove duplicate rows
    subset = extract_tech(technology)[["WorkTitle", feature_name]].drop_duplicates()
    subset = creativeworks[["WorkTitle", feature_name]].drop_duplicates()
    #
    # b) Create a pie chart showing the distribution of values in feature_name
    column_counts = subset[feature_name].value_counts()
    plt.figure(figsize=(10, 7))
    column_counts.plot.pie(
        autopct="%1.1f%%", startangle=140, colors=plt.cm.Paired.colors
    )
    plt.title(f"Distribution of {feature_name} for Drones in Creative Works")
    plt.ylabel("")  # Removing the default y-label for cleaner visuals
    plt.show()


def extract_tech_and_another_variable(technology, feature_name):
    """Filter data

    Args:
        technology (_type_): _description_
        variable_name (_type_): _description_

    Returns:
        _type_: _description_
    """    
    subset = creativeworks[
        (
            creativeworks["TechRef"].str.contains(technology, case=False, na=False)
            | creativeworks["TechUsed"].str.contains(technology, case=False, na=False)
        )
    ][["WorkTitle", feature_name]]
    subset = subset.drop_duplicates()
    feature_counts = subset[feature_name].value_counts()
 # return subset and feature_counts
    return feature_counts, subset


def make_piechart(technology, feature_name):
    subset = extract_tech_and_another_variable(technology, feature_name)
    plt.figure(figsize=(10, 7))
    feature_counts.plot.pie(
        autopct="%1.1f%%", startangle=140, colors=plt.cm.Paired.colors
    )
    plt.title(f"Distribution of {feature_name} for {technology} in Creative Works")
    plt.ylabel("")  # Removing the default y-label for cleaner visuals
    plt.show()


if __name__ == "__main__":

    extract_tech_and_another_variable("Drones", "Topic")
    
    #make_piechart("Drones", "Topic")

    #analyse_tech("Drones", "Topic")

    # analyse_aspect('Drones', 'Topic')


""" # Creating a dictionary to store details for each work in drones_complete
all_works_details = {}

for index, row in drones_complete.iterrows():
    work_title = row['WorkTitle']
    topic = row['Topic']
    genre = row['Genre']
    sentiment = row['Sentiment']

    if work_title not in all_works_details:
        all_works_details[work_title] = {
            'Topics': set(),
            'Genre': genre,
            'Sentiment': set()
        }
    all_works_details[work_title]['Topics'].add(topic)
    all_works_details[work_title]['Sentiment'].add(sentiment)

# Preparing data for the table with all works
all_table_data = []
for work, details in all_works_details.items():
    topics_str = ', '.join(sorted(details['Topics']))
    sentiment_str = ', '.join(sorted(details['Sentiment']))
    all_table_data.append([work, topics_str, details['Genre'], sentiment_str])

# Convert the table data to a Pandas DataFrame for visualization
df_all_works_preview = pd.DataFrame(all_table_data, columns=["WorkTitle", "Topics", "Genre", "Sentiment"])

# Displaying the head of the DataFrame for a preview (showing only first 10 rows for brevity)
df_all_works_preview.head(10) """
