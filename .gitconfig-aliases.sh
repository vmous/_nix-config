#!/bin/shell

# Color Definitions
readonly CL_RED='\033[0;31m'
readonly CL_GREEN='\033[0;32m'
readonly CL_BLUE='\033[0;34m'
readonly CL_CYAN='\033[0;36m'
readonly CL_YELLOW='\033[1;33m'
readonly CL_MAGENTA='\033[0;35m'
readonly CL_NC='\033[0m' # No Color / Reset

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
    if [ -z "${LOCAL_COUNT}" ]; then
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
  if [ -z "$@" ]; then
    echo "Usage: git j-restore-all-but <file1> [<file2> ...]"
    return 1
  fi

  all_modified_files_str=$(git diff --name-only --diff-filter=M)
  local all_modified_files_arr=( $(echo "${all_modified_files_str}") )

  files_to_keep=("$@")

  files_to_restore=()

  for file in "${all_modified_files_arr[@]}"; do
    keep=false
    for keep_file in "${files_to_keep[@]}"; do
      if [ "${file}" = "${keep_file}" ]; then
        keep=true
        break
      fi
    done

    if ! ${keep}; then
      files_to_restore+=("{$file}")
    fi
  done

  total_modified=${#all_modified_files_arr[@]}
  to_restore_count=${#files_to_restore[@]}
  to_keep_count=$((total_modified - to_restore_count))

  echo "--- Git Restore-All-But Summary ---"
  echo "Files to keep:     ${to_keep_count}/${total_modified}"
  for file in "${files_to_keep[@]}"; do
    echo " - ${file}"
  done
  echo ""

  echo "Files to restore:  ${to_restore_count}/${total_modified}"
  for file in "${files_to_restore[@]}"; do
    echo "  - ${file}"
  done
  echo ""


  if [ ${to_restore_count} -gt 0 ]; then
    read -p "Do you want to proceed with restoring these files? (yes/no): " confirmation
    if [[ "$confirmation" =~ ^[Yy][Ee][Ss]$ ]]; then
      echo "Proceeding with restore..."
      CMD="git restore ${files_to_restore[@]}"
      echo "Running: ${CMD}"
      eval ${CMD}
      echo "Restore complete."
    else
      echo "Operation cancelled."
    fi
  else
    echo "No files to restore based on your selection."
  fi
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

# Function to stage all deleted files.
#
# This workflow automates the staging of files that have been deleted
# from the working directory. It specifically targets files marked as
# "deleted" in 'Changes not staged for commit' and ignores any files
# marked as "modified" or "untracked".
#
# Normally, Git users have to run 'git rm <file>' for every deleted path,
# or 'git add -u' which captures both deletions AND modifications.
# This function provides a surgical middle ground.
#
# The function includes safety guards for filenames with spaces/special
# characters and provides a color-coded transparent preview of the
# command being executed.
git_j_stage_deleted() {
    local files=()
    # Use -z to handle spaces/quotes in filenames
    while IFS= read -r -d '' file; do
        files+=("$file")
    done < <(git ls-files --deleted -z)

    if [ ${#files[@]} -eq 0 ]; then
        printf "${CL_YELLOW}No deleted files found to stage.${CL_NC}\n"
        return 0
    fi

    # Build the display string
    local display_str="git rm"
    for f in "${files[@]}"; do
        # 1. Escape existing single quotes: O'Reilly -> O'\''Reilly
        local escaped_f="${f//\'/\'\\\'\'}"
        # 2. Wrap the escaped name in single quotes and Cyan color
        display_str="${display_str} '$escaped_f'"
    done

    # Print the command
    printf "${CL_GREEN}${display_str}${CL_NC}\n"

    git rm -- "${files[@]}"
}
