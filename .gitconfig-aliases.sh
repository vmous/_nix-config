#!/bin/shell

################################################################################
# Constants ####################################################################
################################################################################

# Color Definitions
readonly CL_RED='\033[0;31m'
readonly CL_GREEN='\033[0;32m'
readonly CL_BLUE='\033[0;34m'
readonly CL_CYAN='\033[0;36m'
readonly CL_YELLOW='\033[1;33m'
readonly CL_MAGENTA='\033[0;35m'
readonly CL_NC='\033[0m' # No Color / Reset

################################################################################
# Utility functions ############################################################
################################################################################

# Helper: Collects null-terminated file lists into an array.
# Usage: _collect_files <ArrayName> <GitCommand...>
_collect_files() {
    local -n _target_arr=$1
    shift
    while IFS= read -r -d '' file; do
        _target_arr+=("$file")
    done < <("$@" -z)
}

# Helper: Handles filtering, UI summary, and confirmation logic.
# Usage: _bulk_execute <OpName> <GitCmd> <FilesToProcess_Ref> <IgnoreList_Ref>
_bulk_execute() {
    local op_name="$1"
    local git_cmd="$2"
    local -n __all_files=$3  # Nameref to the array in calling function
    local -n __ignore_list=$4 # Nameref to the array in calling function

    local files_to_process=()
    local files_to_ignore=()

    # Filtering logic
    for f_item in "${__all_files[@]}"; do
        local skip=false
        for ignore_f_item in "${__ignore_list[@]}"; do
            [[ "$f_item" == "$ignore_f_item" ]] && skip=true && break
        done

        if $skip; then
            files_to_ignore+=("$f_item")
        else
            files_to_process+=("$f_item")
        fi
    done

    if [ ${#files_to_process[@]} -eq 0 ]; then
        printf "${CL_YELLOW}All discovered files are excluded. Nothing to $op_name.${CL_NC}\n"
        return 0
    fi

    # Summary UI
    printf "${CL_GREEN}--- $op_name Summary ---${CL_NC}\n"
    if [ ${#files_to_ignore[@]} -gt 0 ]; then
        printf "${CL_YELLOW}Excluded (${#files_to_ignore[@]}/${#__all_files[@]}):${CL_NC}\n"
        for f in "${__ignore_list[@]}"; do
            # Only list the ignore-item if it actually existed in the discovery set
            for found in "${__all_files[@]}"; do
                [[ "$found" == "$f" ]] && printf "${CL_YELLOW}  - $f${CL_NC}\n" && break
            done
        done
    fi

    printf "${CL_RED}To $op_name (${#files_to_process[@]}/${#__all_files[@]}):${CL_NC}\n"
    for f in "${files_to_process[@]}"; do printf "${CL_RED}  - $f${CL_NC}\n"; done

    # Interaction
    printf "\n${CL_CYAN}Do you want to proceed? (yes/no): ${CL_NC}"
    local confirmation
    read -r confirmation
    if [[ "$confirmation" =~ ^[Yy][Ee][Ss]$ ]]; then
        printf "${CL_GREEN}Executing: $git_cmd ...${CL_NC}\n"
        # Split git_cmd into array to handle flags properly
        read -r -a cmd_array <<< "$git_cmd"
        "${cmd_array[@]}" "${files_to_process[@]}"
    else
        printf "${CL_YELLOW}Operation cancelled.${CL_NC}\n"
    fi
}

################################################################################
# Workflows ####################################################################
################################################################################

# "Squash" all local commits into the first local commit reusing first
# commit's message.
git_j_squash_to_first_local() {
    UPSTREAM=$(git rev-parse --abbrev-ref --symbolic-full-name @{u} 2>/dev/null || echo origin/HEAD)
    LAST_REMOTE_ID=$(git merge-base "${UPSTREAM}" HEAD)
    # Check if we actually found a remote ID to avoid the 'ambiguous argument' error
    if [ -z "$LAST_REMOTE_ID" ]; then
        echo "Error: Could not determine upstream merge base."
        return 1
    fi

    FIRST_LOCAL_ID=$(git rev-list --reverse "${LAST_REMOTE_ID}"..HEAD | head -n 1)
    if [ -z "$FIRST_LOCAL_ID" ]; then
        echo "No local commits found to squash."
        return 0
    fi

    FIRST_LOCAL_MSG=$(git log -1 --format=%B "${FIRST_LOCAL_ID}")
    git reset --soft "${LAST_REMOTE_ID}"
    git commit -m "${FIRST_LOCAL_MSG}"
}

# Merge uncommitted files of your working directory into your last local commit.
# By default merges only tracked files (staged or unstaged). Pass the "all"
# argument to merge both tracked and untracked files (if that is what you want;
# since this is an uncommon thing to do, you will be asked to confirm your
# choice). All above are possible only if at least one local-only commit exists.
git_j_uncommited_to_last_local() {
    UPSTREAM=$(git rev-parse --abbrev-ref --symbolic-full-name @{u} 2>/dev/null || echo origin/HEAD)
    LAST_REMOTE_ID=$(git merge-base "${UPSTREAM}" HEAD)
    LOCAL_COUNT=$(git rev-list --count "${LAST_REMOTE_ID}"..HEAD)
    if [ "${LOCAL_COUNT}" -eq 0 ]; then
        echo "Error. No local commits found to merge changes into. Aborting..."
        return 1
    fi

    if [ "${1}" == "all" ]; then
        TRACKED=$(git diff --name-only HEAD)
        UNTRACKED=$(git ls-files --others --exclude-standard)

        if [ -z "${TRACKED}" ] && [ -z "${UNTRACKED}" ]; then
            echo "No changes to squash."
            return 0
        fi

        echo "You are about to squash the following files:"
        [ -n "${TRACKED}" ] && echo -e "\nTracked:\n${TRACKED}"
        [ -n "${UNTRACKED}" ] && echo -e "\nUntracked:\n${UNTRACKED}"

        echo -n -e "\nAre you sure? (y/n): "
        read -r CONFIRM
        if [[ ! "${CONFIRM}" =~ ^[Yy]$ ]]; then
            echo "Aborting..."
            return 1
        fi
        # Add everything
        git add -A
    else
        # Default behavior: Tracked only (staged + unstaged)
        # Check if there are any changes in tracked files
        if git diff --quiet && git diff --cached --quiet; then
            echo "No tracked changes to squash. (Use 'all' to include untracked files)"
            return 0
        fi
        # Only add tracked files that have changed
        git add -u
    fi

    git commit --amend --no-edit
}

git_j_add_all_tracked_but() {
  EXCLUDE_FILES=("$@");
  git diff --name-only --diff-filter=MD | while read -r FILE; do
    EXCLUDE=false;
    for EXCL in "${EXCLUDE_FILES[@]}"; do
      if [[ "$FILE" == "$EXCL" ]]; then
        EXCLUDE=true
        break
      fi
    done
    if ! $EXCLUDE; then
      git add "$FILE";
    fi;
  done;
}

# Function to restore all modified files except a specified list
#
# This workflow restores all the files in the current working
# directory. It gets a list of files as arguments which excludes from
# the files it restores. This workflow helps in cases when you
# accidentally change a lot of files but you cannot restore the whole
# working directory because some of the files contain changes you wish
# to preserve. In that case you need to be more surgical and be able to
# restore only the files you want. For example imagine you have been
# working on a feature that touched 4 files but at some point you
# accidentally execute your project's linter with a faulty configuration
# which in turn touches 150 files of your project; with this workflow
# you can restore all the files except from the 4 files you were working
# on.
git_j_restore_all_but() {
    local discovered=()
    _collect_files discovered git diff --name-only --diff-filter=M

    if [ ${#discovered[@]} -eq 0 ]; then
        printf "${CL_YELLOW}No modified files found.${CL_NC}\n"
        return 0
    fi

    local ignore_list=("$@")
    _bulk_execute "Restore" "git restore" discovered ignore_list
}

# Stage deleted files from the working directory to the staging area.
#
# By default the function operates on all unstaged deleted files. Optionally,
# pass a list of files names to ingore.
#
# This workflow automates the staging of files that have been deleted
# from the working directory. It specifically targets files marked as
# "deleted" in git status' "Changes not staged for commit" section. By design,
# it does not target other files like ones marked as "modified" or "untracked".
#
# Motivation:
# Normally, Git users have to run 'git rm <file>' for every deleted path, or
# 'git add -u' which captures both deletions AND modifications. This function
# offers precise git workflow for staging deleted files by allowing:
# - single-command bulk staging, and
# - flexibility to ignore certain deleted files from staging.
#
# The function includes safety guards for file names with spaces/special
# characters and provides a color-coded transparent preview of the
# command being executed.
git_j_stage_deleted_but() {
    local discovered=()
    _collect_files discovered git ls-files --deleted

    if [ ${#discovered[@]} -eq 0 ]; then
        printf "${CL_YELLOW}No deleted files found.${CL_NC}\n"
        return 0
    fi

    local ignore_list=("$@")
    _bulk_execute "Stage Deletions" "git rm" discovered ignore_list
}

# Unstage deleted files from the staging area back to the working directory.
#
# By default the function operates on all staged deleted files. Optionally, pass
# a list of files names to ingore.
#
# Symmetrical effect to `git_j_stage_deleted_but` above.
git_j_unstage_deleted_but() {
    local discovered=()
    _collect_files discovered git diff --name-only --diff-filter=D --cached

    if [ ${#discovered[@]} -eq 0 ]; then
        printf "${CL_YELLOW}No staged deletions found.${CL_NC}\n"
        return 0
    fi

    local ignore_list=("$@")
    _bulk_execute "Unstage Deletions" "git restore --staged" discovered ignore_list
}
