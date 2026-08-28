# FAIR theory workflow: create, version, publish, and archive a theory.
# Work on one computer; some steps require installing/configuring software.

library(theorytools)

# Goal: create a FAIR theory repository with Git/GitHub, documentation, Zenodo metadata, a release, and a DOI.

# Example: represent Self-Determination Theory (SDT) as a DAG.

theory <- '
dag {
external_event
healthy_development
integration
intrinsic_motivation
locus_of_causality
needs
wellbeing
external_event -> intrinsic_motivation [form="external_event:needs"]
external_event -> locus_of_causality
external_event -> needs
integration -> healthy_development
integration -> wellbeing
intrinsic_motivation -> healthy_development
intrinsic_motivation -> wellbeing
locus_of_causality -> intrinsic_motivation
needs -> integration
needs -> intrinsic_motivation  [form="external_event:needs"]
needs -> wellbeing
}'

# Create the local project folder that will become the theory archive.
project_path <- file.path("~/theories", "sdt")
dir.create(project_path)

# Save the theory as a shareable, machine-readable text file.
writeLines(theory, file.path(project_path, "theory.txt"))

# Add a reuse license; CC0 is recommended here.
worcs::add_license_file(path = project_path, license = "cc0")

# Generate a FAIR-theory README, then edit it with project/interoperability details.
theorytools::add_readme_fair_theory(title = "Self-Determination Theory",
                                    path = project_path)

# Add .zenodo.json metadata so Zenodo can index the theory automatically.
add_zenodo_json_theory(
  path = project_path,
  title = "Self-Determination Theory",
  keywords = c("motivation")
)

# Check that Git is installed, then initialize the project as a Git repository.
worcs::check_git()
gert::git_init(path = project_path)

# Configure GitHub authentication first: create a PAT with usethis::create_github_token(),
# then store it with gitcreds::gitcreds_set().
# Check GitHub access, create a public remote repository, and connect the local project.
worcs::check_github()

worcs::git_remote_create("fair_sdt", private = FALSE)

worcs::git_remote_connect(project_path, remote_repo = "fair_sdt")

# Commit tracked files and push the project to GitHub.
worcs::git_update("First commit of my theory", repo = project_path)


# Publish a GitHub release so Zenodo can archive it.
worcs::git_release_publish(repo = project_path)

# The FAIR theory is now versioned, archived, citable, and discoverable via DOI.
