package org.manhattan.tools.yaychat.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@lombok.Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class Login extends Record{
    @JsonProperty("__rec")
    final String __rec = "login";
    private long mobile;
    private String password;

    @Override
    @JsonProperty("__rec")
    public String getRecordType() {
        return __rec;
    }
}
